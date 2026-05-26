/**
 * EDITOR.JS - Note Editor
 *
 * This module handles the note editor modal.
 * Includes: text formatting, LaTeX templates, image handling, preview.
 */

// ==================== MODAL MANAGEMENT ====================

/**
 * Open the node editor modal for creating a new note
 */
function openNodeModal() {
    State.editingNode = null;
    State.tempImages = [];

    $('nodeTitle').value = '';
    $('nodeTitle').placeholder = 'New Note';
    $('nodeContent').value = '';
    $('flashcardCheck').checked = false;
    $('imageThumbs').innerHTML = '';
    $('previewBox').innerHTML = '';
    $('charCount').textContent = '0';
    $('nodeModal').classList.add('visible');
    $('nodeTitle').focus();
}

/**
 * Open the node editor modal for editing an existing note
 */
function editSelectedNode() {
    if (!State.selectedNode) return;

    closeDetail();
    State.editingNode = State.selectedNode;
    State.tempImages = [...(State.selectedNode.images || [])];

    $('nodeTitle').value = State.selectedNode.title;
    $('nodeTitle').placeholder = 'Untitled';
    $('nodeContent').value = State.selectedNode.content || '';
    $('flashcardCheck').checked = State.selectedNode.flashcard || false;

    renderImageThumbs();
    updatePreview();
    updateCharCount();

    $('nodeModal').classList.add('visible');
    $('nodeContent').focus();
}

/**
 * Close the node editor modal
 */
function closeNodeModal() {
    $('nodeModal').classList.remove('visible');
    State.editingNode = null;
    State.tempImages = [];
}

/**
 * Save the current note
 */
function saveNode() {
    const title = $('nodeTitle').value.trim();
    const content = $('nodeContent').value;
    const flashcard = $('flashcardCheck').checked;

    if (!title) {
        alert('Enter a title');
        return;
    }

    if (State.editingNode) {
        // Update existing node
        State.editingNode.title = title;
        State.editingNode.content = content;
        State.editingNode.flashcard = flashcard;
        State.editingNode.images = State.tempImages;
    } else {
        // Create new node
        let x, y;
        if (State.pendingNodePosition) {
            x = State.pendingNodePosition.x;
            y = State.pendingNodePosition.y;
        } else {
            // Place in center of viewport
            const rect = canvasWrapper.getBoundingClientRect();
            x = 5000 + (rect.width / 2 - State.pan.x) / State.zoom + (Math.random() - 0.5) * 100;
            y = 5000 + (rect.height / 2 - State.pan.y) / State.zoom + (Math.random() - 0.5) * 100;
        }

        State.currentSubtopic.nodes.push({
            id: generateId(),
            title,
            content,
            flashcard,
            images: State.tempImages,
            links: [],
            x,
            y
        });

        State.pendingNodePosition = null;
    }

    saveData();
    renderCanvas();
    renderSidebar();
    closeNodeModal();

    if (State.editingNode) {
        openDetail(State.editingNode);
    }
}

/**
 * Delete the currently selected node
 */
function deleteSelectedNode() {
    if (!State.selectedNode) return;

    // Find node location for potential restore
    const location = findNodeLocation(State.selectedNode.id);

    // Move to trash with metadata
    State.data.trash.push({
        ...State.selectedNode,
        deletedAt: Date.now(),
        fromTopic: location?.topic.id,
        fromSubtopic: location?.subtopic.id
    });

    // Remove links to this node from other nodes
    State.currentSubtopic.nodes.forEach(n => {
        n.links = n.links?.filter(id => id !== State.selectedNode.id) || [];
    });

    // Remove the node
    State.currentSubtopic.nodes = State.currentSubtopic.nodes.filter(n => n.id !== State.selectedNode.id);

    saveData();
    closeDetail();
    State.selectedNode = null;
    renderCanvas();
    renderSidebar();
    showToast('Note moved to trash (30 days)');
}

// ==================== TEXT FORMATTING ====================

/**
 * Insert formatting around selected text
 * @param {string} before - Text to insert before selection
 * @param {string} after - Text to insert after selection
 */
function insertFormat(before, after) {
    const textarea = $('nodeContent');
    const start = textarea.selectionStart;
    const end = textarea.selectionEnd;
    const text = textarea.value;
    const selected = text.substring(start, end) || 'text';

    textarea.value = text.substring(0, start) + before + selected + after + text.substring(end);
    textarea.focus();
    textarea.setSelectionRange(start + before.length, start + before.length + selected.length);

    updatePreview();
    updateCharCount();
}

/**
 * Insert highlight formatting
 * @param {string} color - Highlight color (yellow, green, pink)
 */
function insertHighlight(color) {
    const markers = {
        yellow: ['==', '=='],
        green: ['::', '::'],
        pink: ['~~', '~~']
    };
    const [open, close] = markers[color];
    insertFormat(open, close);
}

/**
 * Insert LaTeX template
 * @param {string} type - Template type (frac, sqrt, int, matrix)
 */
function insertTemplate(type) {
    const templates = {
        frac: '\\frac{num}{den}',
        sqrt: '\\sqrt{x}',
        int: '\\int_{a}^{b} f(x) \\, dx',
        matrix: '\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}'
    };
    insertFormat('$' + templates[type], '$');
}

// ==================== PREVIEW ====================

/**
 * Update the live preview panel
 */
function updatePreview() {
    const text = $('nodeContent').value;
    const box = $('previewBox');
    box.innerHTML = formatContent(text);
    renderMath(box);
}

/**
 * Update character count display
 */
function updateCharCount() {
    const count = $('nodeContent').value.length;
    $('charCount').textContent = count;
}

// ==================== IMAGE HANDLING ====================

/**
 * Initialize image input handler
 */
function initImageInput() {
    const imageInput = $('imageInput');
    if (imageInput) {
        imageInput.onchange = (e) => handleFiles(e.target.files);
    }
}

/**
 * Handle dropped/selected image files
 * @param {FileList} files - Selected files
 */
function handleFiles(files) {
    Array.from(files).forEach(file => {
        if (!file.type.startsWith('image/')) return;

        const reader = new FileReader();
        reader.onload = (e) => {
            State.tempImages.push(e.target.result);
            renderImageThumbs();
        };
        reader.readAsDataURL(file);
    });
}

/**
 * Render image thumbnails in the editor
 */
function renderImageThumbs() {
    const container = $('imageThumbs');
    container.innerHTML = State.tempImages.map((img, i) =>
        `<div class="image-thumb-mini" onclick="removeImage(${i})"><img src="${escapeHtml(img)}"></div>`
    ).join('');
}

/**
 * Remove an image from temporary images
 * @param {number} index - Image index to remove
 */
function removeImage(index) {
    State.tempImages.splice(index, 1);
    renderImageThumbs();
}

// ==================== DETAIL PANEL ====================

/**
 * Select a node and show detail panel
 * @param {Object} node - Node to select
 */
function selectNode(node) {
    State.selectedNode = node;
    renderCanvas();
    openDetail(node);
}

/**
 * Open the detail panel for a node
 * @param {Object} node - Node to display
 */
function openDetail(node) {
    $('detailPanel').classList.add('open');
    $('detailTitle').textContent = node.title;

    // Render content
    const content = $('detailContent');
    let html = formatContent(node.content || '');

    // Add images
    node.images?.forEach(img => {
        html += `<img src="${escapeHtml(img)}">`;
    });

    content.innerHTML = html;
    renderMath(content);

    // Render links
    const linksList = $('linksListDetail');
    const linkedNodes = [
        ...node.links.map(id => State.currentSubtopic.nodes.find(n => n.id === id)).filter(Boolean),
        ...State.currentSubtopic.nodes.filter(n => n.links?.includes(node.id))
    ];
    const unique = [...new Map(linkedNodes.map(n => [n.id, n])).values()];

    linksList.innerHTML = unique.length
        ? unique.map(n =>
            `<span class="link-item" onclick="selectNode(State.currentSubtopic.nodes.find(x=>x.id==='${n.id}'))">${escapeHtml(n.title)}</span>`
        ).join('')
        : '<span style="color:var(--text2)">No links</span>';
}

/**
 * Close the detail panel
 */
function closeDetail() {
    $('detailPanel').classList.remove('open');
    State.selectedNode = null;
    renderCanvas();
}
