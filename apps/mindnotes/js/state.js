/**
 * STATE.JS - Global State Management
 *
 * This module manages all application state and data persistence.
 * It provides the single source of truth for the application.
 */

// ==================== GLOBAL STATE ====================

const State = {
    // Main data structure
    data: { topics: [], trash: [] },

    // Current selection
    currentSubtopic: null,
    selectedNode: null,
    editingNode: null,

    // Temporary data for editing
    tempImages: [],
    pendingNodePosition: null,

    // Canvas state
    pan: { x: 0, y: 0 },
    zoom: 1,

    // Interaction states
    isPanning: false,
    panStart: { x: 0, y: 0 },

    // Linking state
    isLinking: false,
    linkStart: null,
    linkLine: null,
    linkingNode: null,

    // Dragging state
    isDragging: false,
    dragNode: null,
    dragOffset: { x: 0, y: 0 },
    dragStartPos: { x: 0, y: 0 },
    hasDragged: false,
    justFinishedDragging: false,

    // View state
    currentView: 'notes',
    selectedQuizTopics: new Set(),

    // Sync state
    syncId: localStorage.getItem('mindnotes_syncId') || ''
};

// ==================== DATA PERSISTENCE ====================

/**
 * Load data from localStorage
 * Creates default data if none exists
 */
function loadData() {
    const saved = localStorage.getItem('mindnotes2');
    if (saved) {
        State.data = JSON.parse(saved);
        if (!State.data.trash) State.data.trash = [];

        // Clean up trash older than 30 days
        const thirtyDaysAgo = Date.now() - (30 * 24 * 60 * 60 * 1000);
        State.data.trash = State.data.trash.filter(item => item.deletedAt > thirtyDaysAgo);
        saveData();
    } else {
        // Create default data with example content
        State.data = createDefaultData();
    }
}

/**
 * Save data to localStorage
 */
function saveData() {
    localStorage.setItem('mindnotes2', JSON.stringify(State.data));
}

/**
 * Create default data structure with example content
 */
function createDefaultData() {
    return {
        topics: [{
            id: '1',
            name: 'Linear Algebra',
            subtopics: [
                {
                    id: '1.1',
                    name: 'Vector Spaces',
                    nodes: [
                        {
                            id: 'n1',
                            title: 'Definition',
                            content: 'A **vector space** over field $F$ is a set $V$ with:\n\n- Vector addition\n- Scalar multiplication\n\nSatisfying axioms.',
                            x: 5100,
                            y: 5100,
                            flashcard: true,
                            images: [],
                            links: ['n2']
                        },
                        {
                            id: 'n2',
                            title: 'Basis',
                            content: 'A **basis** is a linearly independent spanning set.\n\n$$\\dim(V) = |\\text{basis}|$$',
                            x: 5300,
                            y: 5080,
                            flashcard: true,
                            images: [],
                            links: []
                        },
                        {
                            id: 'n3',
                            title: 'Linear Independence',
                            content: '$\\vec{v}_1,...,\\vec{v}_n$ are linearly independent if:\n\n$$\\sum c_i\\vec{v}_i = 0 \\implies c_i = 0$$',
                            x: 5200,
                            y: 5220,
                            flashcard: false,
                            images: [],
                            links: ['n1']
                        }
                    ]
                },
                { id: '1.2', name: 'Linear Transformations', nodes: [] },
                { id: '1.3', name: 'Eigenvalues', nodes: [] }
            ]
        }],
        trash: []
    };
}

// ==================== STATE HELPERS ====================

/**
 * Get a node by ID from current subtopic
 */
function getNodeById(nodeId) {
    return State.currentSubtopic?.nodes.find(n => n.id === nodeId);
}

/**
 * Find topic and subtopic containing a node
 */
function findNodeLocation(nodeId) {
    for (const topic of State.data.topics) {
        for (const sub of topic.subtopics) {
            if (sub.nodes.some(n => n.id === nodeId)) {
                return { topic, subtopic: sub };
            }
        }
    }
    return null;
}

/**
 * Generate a unique ID
 */
function generateId() {
    return Date.now().toString();
}
