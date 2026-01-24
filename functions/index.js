const functions = require("firebase-functions");
const admin = require("firebase-admin");
const fs = require("fs");
const path = require("path");

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
    const staff = (acl.staff || []).map(e => e.toLowerCase());
    const isStaff = staff.includes(email);
    const playerToken = acl.players ? acl.players[email] : null;

    if (!isStaff && !playerToken) return res.status(403).send("Email non autorizzata");

    const expiresIn = 5 * 24 * 60 * 60 * 1000; // 5 giorni
    const sessionCookie = await admin.auth().createSessionCookie(idToken, { expiresIn });

    // Firebase Hosting lascia passare solo il cookie "__session"
    res.setHeader("Set-Cookie", [
      `__session=${encodeURIComponent(sessionCookie)}; Max-Age=${expiresIn/1000}; Path=/; HttpOnly; Secure; SameSite=Lax`,
    ]);

    return res.status(200).json({ ok: true });
  } catch (e) {
    return res.status(401).send("UNAUTHORIZED");
  }
});

exports.serve = functions.https.onRequest(async (req, res) => {
  try {
    res.setHeader("Cache-Control", "private, no-store");

    const sessionCookie = getCookie(req, "__session");
    if (!sessionCookie) return res.status(302).set("Location", "/login/").send("");

    const decoded = await admin.auth().verifySessionCookie(sessionCookie, true);
    const email = (decoded.email || "").toLowerCase();
    if (!email) return res.status(403).send("Forbidden");

    const acl = loadAcl();
    const staff = (acl.staff || []).map(e => e.toLowerCase());
    const isStaff = staff.includes(email);
    const playerToken = acl.players ? acl.players[email] : null;

    if (!isStaff && !playerToken) return res.status(403).send("Email non autorizzata");

    const pathReq = req.path || "/";

    // player: può vedere SOLO /p/<token>/*
    if (!isStaff) {
      if (pathReq === "/" || pathReq.startsWith("/staff")) {
        return res.status(302).set("Location", `/p/${playerToken}/`).send("");
      }
      const m = pathReq.match(/^\/p\/([^/]+)\//);
      const tokenInPath = m ? m[1] : null;
      if (!tokenInPath || tokenInPath !== playerToken) {
        return res.status(302).set("Location", `/p/${playerToken}/`).send("");
      }
    }

    // Risposta di test
    res.setHeader("Content-Type", "text/html; charset=utf-8");
    return res.status(200).send(`
      <html><body style="font-family:system-ui;max-width:700px;margin:40px auto;padding:0 16px">
        <h1>OK: accesso riuscito</h1>
        <p><b>Email:</b> ${email}</p>
        <p><b>Ruolo:</b> ${isStaff ? "staff" : "player"}</p>
        <p><b>Path richiesto:</b> ${pathReq}</p>
        <p>Prossimo step: servire i file Quarto da Storage privato.</p>
        <p><a href="/login/">Torna al login</a></p>
      </body></html>
    `);
  } catch (e) {
    return res.status(302).set("Location", "/login/").send("");
  }
});
