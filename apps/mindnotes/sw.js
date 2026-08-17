/*
 * MindNotes service worker — "always-fresh app shell".
 *
 * Why this exists: GitHub Pages serves index.html with Cache-Control: max-age=600,
 * so a browser can keep running a 10-minute-old copy of the app after a deploy.
 * There is no way to change that header server-side, so we fix it client-side:
 * for the HTML document we go NETWORK-FIRST and bypass the HTTP cache, so a new
 * deploy is picked up on the very next reload. Offline, we fall back to the last
 * good shell we saw. This SW only ever touches the app shell (HTML) — it never
 * touches IndexedDB, where notes/FSRS data live — so it cannot cause data loss.
 *
 * It intentionally does NOT cache or intercept anything else (CDN assets, seed
 * JSON, the LaTeX API): those pass straight through to the network as before.
 */
const CACHE = 'mindnotes-shell-v2';

self.addEventListener('install', () => {
    // Take over as soon as possible instead of waiting for old tabs to close.
    self.skipWaiting();
});

self.addEventListener('activate', (event) => {
    event.waitUntil((async () => {
        // Drop any stale shell caches from older SW versions.
        const keys = await caches.keys();
        await Promise.all(keys.filter((k) => k !== CACHE).map((k) => caches.delete(k)));
        await self.clients.claim();
    })());
});

self.addEventListener('fetch', (event) => {
    const req = event.request;
    let url;
    try { url = new URL(req.url); } catch (_) { return; }

    // Only manage same-origin navigations / HTML documents. Everything else
    // (CDN scripts, fonts, seed JSON with their own ?t= busting, the LaTeX API)
    // is left completely untouched.
    const isDocument = req.mode === 'navigate' || req.destination === 'document';
    if (req.method !== 'GET' || url.origin !== self.location.origin || !isDocument) {
        return; // fall through to default network handling
    }

    event.respondWith((async () => {
        try {
            // Bypass the HTTP cache so a fresh deploy always wins when online.
            const fresh = await fetch(req.url, { cache: 'no-store' });
            if (fresh && fresh.ok) {
                const cache = await caches.open(CACHE);
                // Normalize the key so ?v=… variants share one offline fallback.
                cache.put('shell', fresh.clone());
            }
            return fresh;
        } catch (err) {
            // Offline → serve the last good shell if we have one.
            const cache = await caches.open(CACHE);
            const cached = await cache.match('shell');
            return cached || Response.error();
        }
    })());
});
