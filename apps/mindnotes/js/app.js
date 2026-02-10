/**
 * APP.JS - Main Application Entry Point
 *
 * This is the main initialization file that ties all modules together.
 * It sets up event listeners and starts the application.
 *
 * Load order is important:
 * 1. state.js    - Global state management
 * 2. utils.js    - Utility functions
 * 3. canvas.js   - Canvas rendering
 * 4. editor.js   - Note editor
 * 5. sidebar.js  - Sidebar & search
 * 6. sync.js     - Cloud sync
 * 7. views.js    - View switching & theme
 * 8. app.js      - This file (initialization)
 */

// ==================== INITIALIZATION ====================

/**
 * Initialize the application
 * Called when DOM is ready
 */
function initApp() {
    // Initialize canvas references
    initCanvasRefs();

    // Set up all event listeners
    initCanvasEvents();
    initKeyboardEvents();
    initImageInput();

    // Load saved data
    loadTheme();
    loadData();

    // Render initial UI
    renderSidebar();
    updateFlashcardStats();

    // Select first subtopic if available
    if (State.data.topics.length && State.data.topics[0].subtopics.length) {
        selectSubtopic(State.data.topics[0], State.data.topics[0].subtopics[0]);
    }

    console.log('MindNotes initialized successfully');
}

// ==================== CANVAS EVENT LISTENERS ====================

/**
 * Initialize all canvas-related event listeners
 */
function initCanvasEvents() {
    // Track panning state for click detection
    let wasPanning = false;
    let panMoved = false;

    // Mouse move handler (dragging, panning, linking)
    document.addEventListener('mousemove', (e) => {
        handleDragMove(e);
        handlePanMove(e);

        // Track if we've moved during panning
        if (wasPanning && State.isPanning) {
            panMoved = true;
        }

        // Update link line if linking
        if (State.isLinking && State.linkLine) {
            const rect = canvasWrapper.getBoundingClientRect();
            const x = (e.clientX - rect.left - State.pan.x) / State.zoom + 5000;
            const y = (e.clientY - rect.top - State.pan.y) / State.zoom + 5000;
            State.linkLine.setAttribute('x2', x);
            State.linkLine.setAttribute('y2', y);
        }
    });

    // Mouse up handler
    document.addEventListener('mouseup', (e) => {
        endDragging();
        endPanning();
    });

    // Canvas wrapper mousedown (start panning)
    canvasWrapper.addEventListener('mousedown', (e) => {
        const isCanvasClick = e.target === canvasWrapper ||
                              e.target.id === 'canvasBg' ||
                              e.target === canvas ||
                              e.target === svg;

        if (isCanvasClick) {
            wasPanning = true;
            panMoved = false;
            startPanning(e);
        }
    });

    // Canvas wrapper click (create node)
    canvasWrapper.addEventListener('click', (e) => {
        const isCanvasClick = e.target === canvasWrapper ||
                              e.target.id === 'canvasBg' ||
                              e.target === canvas ||
                              e.target === svg;

        if (isCanvasClick) {
            // Don't create node if we were panning
            if (panMoved) {
                wasPanning = false;
                panMoved = false;
                return;
            }
            wasPanning = false;
            handleCanvasClick(e);
        }
    });

    // Wheel zoom
    canvasWrapper.addEventListener('wheel', handleWheelZoom);

    // Close color pickers when clicking elsewhere
    document.addEventListener('click', (e) => {
        if (!e.target.closest('.color-picker') && !e.target.closest('.node-color-btn')) {
            document.querySelectorAll('.color-picker.open').forEach(p => p.classList.remove('open'));
        }
    });
}

// ==================== KEYBOARD EVENT LISTENERS ====================

/**
 * Initialize keyboard shortcuts
 */
function initKeyboardEvents() {
    document.addEventListener('keydown', (e) => {
        // Escape key - close modals and cancel operations
        if (e.key === 'Escape') {
            cancelLinking();
            closeNodeModal();
            closeTopicModal();
            closeSubtopicModal();
            closeSyncModal();
            closeDetail();
        }

        // Delete/Backspace - delete selected node
        const modalOpen = $('nodeModal').classList.contains('visible');
        if ((e.key === 'Delete' || e.key === 'Backspace') && State.selectedNode && !modalOpen) {
            e.preventDefault();
            deleteSelectedNode();
        }

        // Editor shortcuts
        if (modalOpen) {
            const textarea = $('nodeContent');
            const titleInput = $('nodeTitle');

            if (document.activeElement === textarea || document.activeElement === titleInput) {
                if (e.ctrlKey || e.metaKey) {
                    // Allow native undo/redo
                    if (e.key === 'z' || e.key === 'y') return;

                    // Bold
                    if (e.key === 'b' && document.activeElement === textarea) {
                        e.preventDefault();
                        insertFormat('**', '**');
                    }

                    // Italic
                    if (e.key === 'i' && document.activeElement === textarea) {
                        e.preventDefault();
                        insertFormat('*', '*');
                    }
                }
            }
        }
    });
}

// ==================== DOM READY ====================

// Start app when DOM is loaded
document.addEventListener('DOMContentLoaded', initApp);
