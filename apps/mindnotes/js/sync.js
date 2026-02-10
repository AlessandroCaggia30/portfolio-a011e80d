/**
 * SYNC.JS - Cloud Synchronization
 *
 * This module handles cloud sync functionality using jsonblob.com.
 * Includes: encryption/decryption, upload, download.
 *
 * NOTE: The encryption used here is basic XOR cipher.
 * For production use, implement proper AES encryption.
 */

// ==================== CONFIGURATION ====================

const SYNC_API = 'https://jsonblob.com/api/jsonBlob';

// ==================== MODAL MANAGEMENT ====================

/**
 * Open the sync modal
 */
function openSyncModal() {
    $('syncPassword').value = '';
    $('syncId').value = State.syncId;
    $('currentSyncId').textContent = State.syncId || 'Not set';
    $('syncResult').style.display = 'none';
    $('syncModal').classList.add('visible');
}

/**
 * Close the sync modal
 */
function closeSyncModal() {
    $('syncModal').classList.remove('visible');
}

// ==================== ENCRYPTION ====================

/**
 * Encrypt data with a password (basic XOR cipher)
 * @param {Object} data - Data to encrypt
 * @param {string} password - Encryption password
 * @returns {string} Encrypted and base64 encoded string
 */
function encrypt(data, password) {
    const str = JSON.stringify(data);
    let hash = 0;

    // Generate hash from password
    for (let i = 0; i < password.length; i++) {
        hash = ((hash << 5) - hash) + password.charCodeAt(i);
        hash = hash & hash;
    }

    const key = Math.abs(hash).toString();
    let result = '';

    // XOR each character
    for (let i = 0; i < str.length; i++) {
        result += String.fromCharCode(str.charCodeAt(i) ^ key.charCodeAt(i % key.length));
    }

    return btoa(encodeURIComponent(result));
}

/**
 * Decrypt data with a password
 * @param {string} encoded - Encrypted base64 string
 * @param {string} password - Decryption password
 * @returns {Object|null} Decrypted data or null if failed
 */
function decrypt(encoded, password) {
    try {
        const str = decodeURIComponent(atob(encoded));
        let hash = 0;

        // Generate hash from password
        for (let i = 0; i < password.length; i++) {
            hash = ((hash << 5) - hash) + password.charCodeAt(i);
            hash = hash & hash;
        }

        const key = Math.abs(hash).toString();
        let result = '';

        // XOR each character
        for (let i = 0; i < str.length; i++) {
            result += String.fromCharCode(str.charCodeAt(i) ^ key.charCodeAt(i % key.length));
        }

        return JSON.parse(result);
    } catch (e) {
        return null;
    }
}

// ==================== CLOUD OPERATIONS ====================

/**
 * Upload data to cloud storage
 */
async function uploadToCloud() {
    const password = $('syncPassword').value;
    const existingId = $('syncId').value.trim();
    const resultDiv = $('syncResult');

    // Validate password
    if (!password) {
        showSyncResult('Please enter a password', false);
        return;
    }

    // Encrypt and prepare payload
    const encrypted = encrypt(State.data, password);
    const payload = { data: encrypted, timestamp: Date.now() };

    try {
        $('syncBtn').classList.add('syncing');

        let response;
        if (existingId) {
            // Update existing blob
            response = await fetch(`${SYNC_API}/${existingId}`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(payload)
            });
            State.syncId = existingId;
        } else {
            // Create new blob
            response = await fetch(SYNC_API, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(payload)
            });
            State.syncId = response.headers.get('X-jsonblob') || response.url.split('/').pop();
        }

        if (response.ok) {
            // Save sync ID
            localStorage.setItem('mindnotes_syncId', State.syncId);
            $('syncId').value = State.syncId;
            $('currentSyncId').textContent = State.syncId;

            // Update UI
            $('syncBtn').classList.add('synced');
            $('syncStatus').classList.add('active');
            $('syncStatusText').textContent = 'Synced: ' + new Date().toLocaleTimeString();

            showSyncResult(
                `Uploaded successfully!<br><br><strong>Your Sync ID:</strong> ${State.syncId}<br><small>Save this ID and your password to restore on another device.</small>`,
                true
            );
        } else {
            throw new Error('Upload failed');
        }
    } catch (e) {
        showSyncResult('Upload failed. Try again.', false);
    } finally {
        $('syncBtn').classList.remove('syncing');
    }
}

/**
 * Download data from cloud storage
 */
async function downloadFromCloud() {
    const password = $('syncPassword').value;
    const id = $('syncId').value.trim();

    // Validate inputs
    if (!password || !id) {
        showSyncResult('Please enter both Sync ID and password', false);
        return;
    }

    try {
        $('syncBtn').classList.add('syncing');

        // Fetch data
        const response = await fetch(`${SYNC_API}/${id}`);
        if (!response.ok) throw new Error('Not found');

        // Decrypt
        const payload = await response.json();
        const decrypted = decrypt(payload.data, password);

        if (!decrypted || !decrypted.topics) {
            showSyncResult('Wrong password or corrupted data', false);
            return;
        }

        // Apply data
        State.data = decrypted;
        saveData();
        State.syncId = id;
        localStorage.setItem('mindnotes_syncId', State.syncId);

        // Refresh UI
        renderSidebar();
        updateFlashcardStats();

        if (State.data.topics.length && State.data.topics[0].subtopics.length) {
            selectSubtopic(State.data.topics[0], State.data.topics[0].subtopics[0]);
        }

        // Update sync status
        $('currentSyncId').textContent = State.syncId;
        $('syncBtn').classList.add('synced');
        $('syncStatus').classList.add('active');
        $('syncStatusText').textContent = 'Restored: ' + new Date().toLocaleTimeString();

        showSyncResult(
            `Notes restored successfully!<br><small>Found ${State.data.topics.length} topics.</small>`,
            true
        );
    } catch (e) {
        showSyncResult('Could not find or decrypt data. Check your Sync ID.', false);
    } finally {
        $('syncBtn').classList.remove('syncing');
    }
}

/**
 * Display sync result message
 * @param {string} message - Message to display
 * @param {boolean} success - Whether operation was successful
 */
function showSyncResult(message, success) {
    const resultDiv = $('syncResult');
    resultDiv.innerHTML = (success ? '✅ ' : '❌ ') + message;
    resultDiv.style.color = success ? 'var(--green)' : 'var(--red)';
    resultDiv.style.display = 'block';
}
