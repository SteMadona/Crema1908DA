const functions = require("firebase-functions");
const admin = require("firebase-admin");
const fs = require("fs");
const path = require("path");

const { Storage } = require("@google-cloud/storage");
const mime = require("mime-types");

const storage = new Storage();
const bucketName = "crema1908dataanalysis-1c14b.firebasestorage.app";

admin.initializeApp();

// Allowlist locale (per test)
const ACL_PATH = path.join(__dirname, "acl.json");
function loadAcl() {
  const raw = fs.readFileSync(ACL_PATH, "utf8");
  return JSON.parse(raw);
}

function getCookie(req, name) {
  const cookie = req.headers.cookie || "";
  const match = cookie.match(new RegExp(`${name}=([^;]+)`));
  return match ? decodeURIComponent(match[1]) : null;
}

// POST /api/sessionLogin  body: { idToken }
exports.sessionLogin = functions.https.onRequest(async (req, res) => {
  try {
    const idToken = req.body?.idToken;
    if (!idToken) return res.status(400).send("Missing idToken");

    const decoded = await admin.auth().verifyIdToken(idToken);
    const email = (decoded.email || "").toLowerCase();
    if (!email) return res.status(401).send("No email");

    const acl = loadAcl();
    const staff = (acl.staff || []).map((e) => e.toLowerCase());
    const isStaff = staff.includes(email);
    const playerToken = acl.players ? acl.players[email] : null;

    if (!isStaff && !playerToken)
      return res.status(403).send("Email non autorizzata");

    const expiresIn = 5 * 24 * 60 * 60 * 1000; // 5 giorni
    const sessionCookie = await admin
      .auth()
      .createSessionCookie(idToken, { expiresIn });

    // Firebase Hosting lascia passare solo il cookie "__session"
    res.setHeader("Set-Cookie", [
      `__session=${encodeURIComponent(
        sessionCookie
      )}; Max-Age=${expiresIn / 1000}; Path=/; HttpOnly; Secure; SameSite=Lax`,
    ]);

    return res.status(200).json({ ok: true });
  } catch (e) {
    console.error(e);
    return res.status(401).send("UNAUTHORIZED");
  }
});

exports.serve = functions.https.onRequest(async (req, res) => {
  try {
    // marker deploy + no-cache per HTML
    const BUILD_REV = process.env.BUILD_REV || "dev";
    res.setHeader("X-Serve-Rev", BUILD_REV);

    if (req.path === "/__version") {
      return res.status(200).json({ ok: true, buildRev: BUILD_REV });
    }

    const sessionCookie = getCookie(req, "__session");
    if (!sessionCookie)
      return res.status(302).set("Location", "/login/").send("");

    const decoded = await admin.auth().verifySessionCookie(sessionCookie, true);
    const email = (decoded.email || "").toLowerCase();
    if (!email) return res.status(403).send("Forbidden");

    const acl = loadAcl();
    const staff = (acl.staff || []).map((e) => e.toLowerCase());
    const isStaff = staff.includes(email);
    const playerToken = acl.players ? acl.players[email] : null;

    if (!isStaff && !playerToken)
      return res.status(403).send("Email non autorizzata");

    const pathReq = req.path || "/";

    // player: può vedere /p/<token>/* + assets globali necessari
    if (!isStaff) {
      const isAllowedAsset =
        pathReq === "/styles.css" ||
        pathReq.startsWith("/site_libs/") ||
        pathReq.startsWith("/images/") ||
        pathReq === "/favicon.ico";

      if (!isAllowedAsset) {
        if (pathReq === "/" || pathReq.startsWith("/staff")) {
          return res
            .status(302)
            .set("Location", `/p/${playerToken}/`)
            .send("");
        }
        const m = pathReq.match(/^\/p\/([^/]+)\//);
        const tokenInPath = m ? m[1] : null;
        if (!tokenInPath || tokenInPath !== playerToken) {
          return res
            .status(302)
            .set("Location", `/p/${playerToken}/`)
            .send("");
        }
      }
    }

    // ---- SERVE FILE DA STORAGE (contenuto Quarto in /protected) ----
    let objectPath;

    // "/" -> protected/index.html
    if (pathReq === "/" || pathReq === "") {
      objectPath = "protected/index.html";
    } else if (pathReq.endsWith("/")) {
      objectPath = `protected${pathReq}index.html`;
    } else {
      objectPath = `protected${pathReq}`;
    }

    const file = storage.bucket(bucketName).file(objectPath);
    const [exists] = await file.exists();
    if (!exists) return res.status(404).send("Not found");

    // Content-Type robusto (evita download)
    const ext = path.extname(objectPath).toLowerCase();
    let contentType =
      ext === ".html"
        ? "text/html; charset=utf-8"
        : ext === ".css"
        ? "text/css; charset=utf-8"
        : ext === ".js"
        ? "application/javascript; charset=utf-8"
        : ext === ".json"
        ? "application/json; charset=utf-8"
        : mime.contentType(ext) ||
          mime.contentType(objectPath) ||
          "application/octet-stream";

    res.setHeader("Content-Type", contentType);
    res.setHeader("Content-Disposition", "inline");

    // cache: HTML no-store, asset un po' cache
    if (ext === ".html") {
      res.setHeader("Cache-Control", "private, no-store");
    } else {
      res.setHeader("Cache-Control", "private, max-age=86400"); // 1 giorno
    }

    return file
      .createReadStream()
      .on("error", (err) => {
        console.error("Storage stream error:", err);
        if (!res.headersSent) res.status(500);
        res.end("Internal error");
      })
      .pipe(res);
  } catch (e) {
    console.error(e);
    return res.status(500).send("Internal error");
  }
});