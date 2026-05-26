/**
 * CANVAS.JS - Canvas Rendering & Interactions
 *
 * This module handles the infinite canvas where notes are displayed.
 * Includes: rendering nodes, drawing links, panning, zooming, drag & drop.
 */

// ==================== DOM REFERENCES ====================

let canvas, canvasWrapper, svg;

/**
 * Initialize canvas DOM references
 * Called once on app startup
 */
function initCanvasRefs() {
    canvas = $('canvas');
    canvasWrapper = $('canvasWrapper');
    svg = $('linksSvg');
}

// ==================== CANVAS RENDERING ====================

/**
 * Render the entire canvas with nodes and links
 * Called when subtopic changes or nodes are modified
 */
function renderCanvas() {
    // Clear existing nodes
    canvas.querySelectorAll('.node').forEach(n => n.remove());
    svg.innerHTML = '';

    if (!State.currentSubtopic) return;

    // Draw links first (behind nodes)
    State.currentSubtopic.nodes.forEach(node => {
        node.links?.forEach(targetId => {
            const target = State.currentSubtopic.nodes.find(n => n.id === targetId);
            if (target) {
                drawLink(node, target);
            }
        });
    });

    // Draw nodes
    State.currentSubtopic.nodes.forEach(node => {
        createNodeElement(node);
    });
}

/**
 * Re-render only the links (for performance during drag)
 */
function renderLinks() {
    svg.innerHTML = '';
    State.currentSubtopic?.nodes.forEach(node => {
        node.links?.forEach(targetId => {
            const target = State.currentSubtopic.nodes.find(n => n.id === targetId);
            if (target) drawLink(node, target);
        });
    });
    if (State.linkLine) svg.appendChild(State.linkLine);
}

// ==================== NODE ELEMENT CREATION ====================

/**
 * Create a DOM element for a node
 * @param {Object} node - Node data object
 */
function createNodeElement(node) {
    const el = document.createElement('div');
    el.className = 'node' + (State.selectedNode?.id === node.id ? ' selected' : '');
    el.dataset.id = node.id;
    el.style.left = node.x + 'px';
    el.style.top = node.y + 'px';
    if (node.width) el.style.width = node.width + 'px';
    if (node.height) el.style.height = node.height + 'px';

    // Apply color as left border
    if (node.color && NODE_COLORS[node.color]) {
        el.style.borderLeftColor = NODE_COLORS[node.color];
        el.style.borderLeftWidth = '4px';
    }

    // Generate preview text (stripped of formatting)
    const previewText = (node.content || '')
        .replace(/[*_`#$=:~\\\[\]]/g, '')
        .replace(/\n/g, ' ')
        .substring(0, 80);

    // Build node HTML
    el.innerHTML = `
        <div class="node-color-btn" style="background:${NODE_COLORS[node.color] || 'var(--border)'}"></div>
        <div class="color-picker" id="picker-${node.id}">
            <div class="color-opt" style="background:var(--border)" data-c="none"></div>
            ${Object.entries(NODE_COLORS).map(([name, color]) =>
                `<div class="color-opt" style="background:${color}" data-c="${name}"></div>`
            ).join('')}
        </div>
        <div class="node-title">${escapeHtml(node.title || 'Untitled')}</div>
        ${previewText ? `<div class="node-preview">${escapeHtml(previewText)}</div>` : ''}
        <div class="resize-handle"></div>
    `;

    // Attach event listeners
    attachNodeEventListeners(el, node);

    canvas.appendChild(el);
}

/**
 * Attach all event listeners to a node element
 * @param {HTMLElement} el - Node DOM element
 * @param {Object} node - Node data object
 */
function attachNodeEventListeners(el, node) {
    // Color button click
    el.querySelector('.node-color-btn').addEventListener('click', (e) => {
        e.stopPropagation();
        document.querySelectorAll('.color-picker.open').forEach(p => p.classList.remove('open'));
        el.querySelector('.color-picker').classList.toggle('open');
    });

    // Color option selection
    el.querySelectorAll('.color-opt').forEach(opt => {
        opt.addEventListener('click', (e) => {
            e.stopPropagation();
            const color = opt.dataset.c;
            node.color = color === 'none' ? null : color;
            saveData();
            renderCanvas();
        });
    });

    // Resize handle
    attachResizeHandler(el, node);

    // Track click timing for single-click vs drag detection
    let clickStartTime = 0;
    let clickStartPos = { x: 0, y: 0 };

    // Mousedown for drag and link
    el.addEventListener('mousedown', (e) => {
        if (e.target.closest('.node-color-btn') || e.target.closest('.color-picker') || e.target.closest('.resize-handle')) {
            return;
        }
        clickStartTime = Date.now();
        clickStartPos = { x: e.clientX, y: e.clientY };
        handleNodeMousedown(e, el, node);
    });

    // Click to edit (if it was a quick click without much movement)
    el.addEventListener('click', (e) => {
        if (e.target.closest('.node-color-btn') || e.target.closest('.color-picker') || e.target.closest('.resize-handle')) {
            return;
        }

        const clickDuration = Date.now() - clickStartTime;
        const dx = Math.abs(e.clientX - clickStartPos.x);
        const dy = Math.abs(e.clientY - clickStartPos.y);

        // If it was a quick click (< 300ms) and didn't move much (< 5px), and not linking
        if (clickDuration < 300 && dx < 5 && dy < 5 && !State.linkingNode) {
            e.stopPropagation();
            State.selectedNode = node;
            editSelectedNode();
        }
    });

    // Double-click as backup
    el.addEventListener('dblclick', (e) => {
        if (e.target.closest('.node-color-btn') || e.target.closest('.color-picker') || e.target.closest('.resize-handle')) return;
        e.preventDefault();
        e.stopPropagation();
        State.selectedNode = node;
        editSelectedNode();
    });
}

/**
 * Attach resize handler to node
 * @param {HTMLElement} el - Node DOM element
 * @param {Object} node - Node data object
 */
function attachResizeHandler(el, node) {
    const resizeHandle = el.querySelector('.resize-handle');
    resizeHandle.addEventListener('mousedown', (e) => {
        e.stopPropagation();
        e.preventDefault();
        const startX = e.clientX;
        const startY = e.clientY;
        const startW = el.offsetWidth;
        const startH = el.offsetHeight;

        function onMove(e) {
            const w = Math.max(120, startW + (e.clientX - startX) / State.zoom);
            const h = Math.max(60, startH + (e.clientY - startY) / State.zoom);
            el.style.width = w + 'px';
            el.style.height = h + 'px';
            node.width = w;
            node.height = h;
        }

        function onUp() {
            document.removeEventListener('mousemove', onMove);
            document.removeEventListener('mouseup', onUp);
            saveData();
            renderLinks();
        }

        document.addEventListener('mousemove', onMove);
        document.addEventListener('mouseup', onUp);
    });
}

/**
 * Handle mousedown on a node
 * @param {MouseEvent} e - Mouse event
 * @param {HTMLElement} el - Node DOM element
 * @param {Object} node - Node data object
 */
function handleNodeMousedown(e, el, node) {
    if (e.target.closest('.node-color-btn') || e.target.closest('.color-picker') || e.target.closest('.resize-handle')) {
        return;
    }

    // Command/Ctrl + click for linking
    if (e.metaKey || e.ctrlKey) {
        e.preventDefault();
        e.stopPropagation();
        handleLinkingClick(el, node);
        return;
    }

    // Complete link if in linking mode
    if (State.linkingNode && State.linkingNode.id !== node.id) {
        completeLinking(node);
        return;
    }

    // Start dragging
    startDragging(e, el, node);
}

// ==================== LINKING ====================

/**
 * Handle Cmd+click for linking nodes
 * @param {HTMLElement} el - Node DOM element
 * @param {Object} node - Node data object
 */
function handleLinkingClick(el, node) {
    if (State.linkingNode && State.linkingNode.id !== node.id) {
        // Complete the link
        completeLinking(node);
    } else if (!State.linkingNode) {
        // Start linking
        State.linkingNode = node;
        el.classList.add('linking-source');
        document.querySelectorAll('.node').forEach(n => {
            if (n.dataset.id !== node.id) n.classList.add('link-target');
        });
        showToast('Cmd+click another note to link');
    }
}

/**
 * Complete a link between two nodes
 * @param {Object} targetNode - Target node to link to
 */
function completeLinking(targetNode) {
    if (!State.linkingNode.links) State.linkingNode.links = [];

    if (!State.linkingNode.links.includes(targetNode.id)) {
        State.linkingNode.links.push(targetNode.id);
        saveData();
        showToast('Link created!');
    } else {
        showToast('Link already exists');
    }

    // Reset linking state
    document.querySelectorAll('.node').forEach(n => n.classList.remove('linking-source', 'link-target'));
    State.linkingNode = null;
    renderCanvas();
}

/**
 * Cancel linking mode
 */
function cancelLinking() {
    if (State.linkingNode) {
        document.querySelectorAll('.node').forEach(n => n.classList.remove('linking-source', 'link-target'));
        State.linkingNode = null;
        showToast('Linking cancelled');
    }
}

// ==================== DRAGGING ====================

/**
 * Start dragging a node
 * @param {MouseEvent} e - Mouse event
 * @param {HTMLElement} el - Node DOM element
 * @param {Object} node - Node data object
 */
function startDragging(e, el, node) {
    State.isDragging = true;
    State.hasDragged = false;
    State.dragNode = node;
    State.dragStartPos = { x: e.clientX, y: e.clientY };

    const rect = canvasWrapper.getBoundingClientRect();
    State.dragOffset.x = (e.clientX - rect.left - State.pan.x) / State.zoom + 5000 - node.x;
    State.dragOffset.y = (e.clientY - rect.top - State.pan.y) / State.zoom + 5000 - node.y;
    el.style.zIndex = 100;
}

/**
 * Handle mouse move for dragging
 * @param {MouseEvent} e - Mouse event
 */
function handleDragMove(e) {
    if (!State.isDragging || !State.dragNode) return;

    const dx = Math.abs(e.clientX - State.dragStartPos.x);
    const dy = Math.abs(e.clientY - State.dragStartPos.y);

    if (dx > 5 || dy > 5) {
        State.hasDragged = true;
    }

    if (State.hasDragged) {
        const rect = canvasWrapper.getBoundingClientRect();
        const x = (e.clientX - rect.left - State.pan.x) / State.zoom - State.dragOffset.x + 5000;
        const y = (e.clientY - rect.top - State.pan.y) / State.zoom - State.dragOffset.y + 5000;
        State.dragNode.x = x;
        State.dragNode.y = y;

        const el = document.querySelector(`.node[data-id="${State.dragNode.id}"]`);
        if (el) {
            el.style.left = x + 'px';
            el.style.top = y + 'px';
        }
        renderLinks();
    }
}

/**
 * End dragging
 */
function endDragging() {
    if (State.isDragging) {
        State.justFinishedDragging = State.hasDragged;
        State.isDragging = false;

        if (State.hasDragged) {
            saveData();
        }
        State.hasDragged = false;
        State.dragNode = null;
    }
}

// ==================== LINK DRAWING ====================

/**
 * Draw a link line between two nodes
 * @param {Object} from - Source node
 * @param {Object} to - Target node
 */
function drawLink(from, to) {
    const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
    const fromW = from.width || 160;
    const fromH = from.height || 70;
    const toW = to.width || 160;
    const toH = to.height || 70;

    line.setAttribute('x1', from.x + fromW / 2);
    line.setAttribute('y1', from.y + fromH / 2);
    line.setAttribute('x2', to.x + toW / 2);
    line.setAttribute('y2', to.y + toH / 2);
    line.setAttribute('stroke', '#6c5ce7');  // accent color - SVG doesn't support CSS vars
    line.setAttribute('stroke-width', '2');
    line.setAttribute('stroke-opacity', '0.6');

    svg.appendChild(line);
}

// ==================== PANNING ====================

/**
 * Start panning the canvas
 * @param {MouseEvent} e - Mouse event
 */
function startPanning(e) {
    State.isPanning = true;
    State.panStart.x = e.clientX - State.pan.x;
    State.panStart.y = e.clientY - State.pan.y;
    canvas.classList.add('grabbing');
}

/**
 * Handle pan movement
 * @param {MouseEvent} e - Mouse event
 */
function handlePanMove(e) {
    if (!State.isPanning) return;
    State.pan.x = e.clientX - State.panStart.x;
    State.pan.y = e.clientY - State.panStart.y;
    updateTransform();
}

/**
 * End panning
 */
function endPanning() {
    if (State.isPanning) {
        State.isPanning = false;
        canvas.classList.remove('grabbing');
    }
}

// ==================== ZOOMING ====================

/**
 * Zoom in
 */
function zoomIn() {
    State.zoom = Math.min(State.zoom * 1.2, 3);
    updateTransform();
}

/**
 * Zoom out
 */
function zoomOut() {
    State.zoom = Math.max(State.zoom / 1.2, 0.2);
    updateTransform();
}

/**
 * Handle wheel zoom
 * @param {WheelEvent} e - Wheel event
 */
function handleWheelZoom(e) {
    e.preventDefault();
    const delta = e.deltaY > 0 ? 0.9 : 1.1;
    const newZoom = Math.min(Math.max(State.zoom * delta, 0.2), 3);

    const rect = canvasWrapper.getBoundingClientRect();
    const mx = e.clientX - rect.left;
    const my = e.clientY - rect.top;

    // Zoom towards mouse position
    State.pan.x = mx - (mx - State.pan.x) * (newZoom / State.zoom);
    State.pan.y = my - (my - State.pan.y) * (newZoom / State.zoom);
    State.zoom = newZoom;

    updateTransform();
}

/**
 * Update canvas transform (pan and zoom)
 */
function updateTransform() {
    canvas.style.transform = `translate(${State.pan.x}px, ${State.pan.y}px) scale(${State.zoom})`;
    $('canvasBg').style.backgroundPosition = `${State.pan.x}px ${State.pan.y}px`;
    $('canvasBg').style.backgroundSize = `${40 * State.zoom}px ${40 * State.zoom}px`;
    $('zoomLevel').textContent = Math.round(State.zoom * 100) + '%';
}

/**
 * Fit view to show all nodes
 */
function fitView() {
    if (!State.currentSubtopic?.nodes.length) {
        State.pan = { x: 0, y: 0 };
        State.zoom = 1;
        updateTransform();
        return;
    }

    const nodes = State.currentSubtopic.nodes;
    const minX = Math.min(...nodes.map(n => n.x));
    const maxX = Math.max(...nodes.map(n => n.x)) + 200;
    const minY = Math.min(...nodes.map(n => n.y));
    const maxY = Math.max(...nodes.map(n => n.y)) + 60;

    const rect = canvasWrapper.getBoundingClientRect();
    const padding = 100;

    const scaleX = (rect.width - padding * 2) / (maxX - minX);
    const scaleY = (rect.height - padding * 2) / (maxY - minY);
    State.zoom = Math.min(scaleX, scaleY, 1.5);
    State.zoom = Math.max(State.zoom, 0.3);

    const centerX = (minX + maxX) / 2;
    const centerY = (minY + maxY) / 2;

    State.pan.x = rect.width / 2 - (centerX - 5000) * State.zoom;
    State.pan.y = rect.height / 2 - (centerY - 5000) * State.zoom;

    updateTransform();
}

// ==================== CANVAS CLICK (CREATE NODE) ====================

/**
 * Handle click on empty canvas to create node
 * @param {MouseEvent} e - Mouse event
 */
function handleCanvasClick(e) {
    // Cancel linking if active
    if (State.linkingNode) {
        cancelLinking();
        return;
    }

    if (!State.currentSubtopic) {
        alert('Select a subtopic first');
        return;
    }

    const rect = canvasWrapper.getBoundingClientRect();
    const x = (e.clientX - rect.left - State.pan.x) / State.zoom + 5000;
    const y = (e.clientY - rect.top - State.pan.y) / State.zoom + 5000;

    State.pendingNodePosition = { x, y };
    openNodeModal();
}
