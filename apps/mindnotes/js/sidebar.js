/**
 * SIDEBAR.JS - Sidebar, Topics & Search
 *
 * This module handles the sidebar UI including:
 * topic/subtopic management, search functionality, and navigation.
 */

// ==================== SIDEBAR RENDERING ====================

/**
 * Render the sidebar with topics and subtopics
 */
function renderSidebar() {
    const container = $('topicsSection');
    container.innerHTML = '';

    State.data.topics.forEach(topic => {
        const group = document.createElement('div');
        group.className = 'topic-group expanded';
        group.innerHTML = `
            <div class="topic-header expanded" onclick="toggleTopic(this)">
                <span class="arrow">&#9654;</span>
                <span class="name">${escapeHtml(topic.name)}</span>
                <div class="topic-actions">
                    <button onclick="event.stopPropagation(); addSubtopicTo('${topic.id}')" title="Add subtopic">+</button>
                </div>
            </div>
            <div class="subtopics"></div>
        `;

        const subtopicsDiv = group.querySelector('.subtopics');
        topic.subtopics.forEach(sub => {
            const item = document.createElement('div');
            item.className = 'subtopic-item' + (State.currentSubtopic?.id === sub.id ? ' active' : '');
            item.innerHTML = `
                <span>${escapeHtml(sub.name)}</span>
                <span class="count">${sub.nodes.length}</span>
            `;
            item.onclick = () => selectSubtopic(topic, sub);
            subtopicsDiv.appendChild(item);
        });

        container.appendChild(group);
    });
}

/**
 * Toggle topic expansion
 * @param {HTMLElement} header - Topic header element
 */
function toggleTopic(header) {
    header.classList.toggle('expanded');
    header.parentElement.classList.toggle('expanded');
}

/**
 * Select a subtopic and load its notes
 * @param {Object} topic - Parent topic
 * @param {Object} subtopic - Subtopic to select
 */
function selectSubtopic(topic, subtopic) {
    State.currentSubtopic = subtopic;
    renderSidebar();
    renderCanvas();
    closeDetail();
    fitView();
}

// ==================== TOPIC MANAGEMENT ====================

// Temporary storage for adding subtopics
let currentTopicForSubtopic = null;

/**
 * Open dialog to add a subtopic to a topic
 * @param {string} topicId - Parent topic ID
 */
function addSubtopicTo(topicId) {
    currentTopicForSubtopic = State.data.topics.find(t => t.id === topicId);
    $('subtopicNameInput').value = '';
    $('subtopicModal').classList.add('visible');
    $('subtopicNameInput').focus();
}

/**
 * Open the new topic modal
 */
function openTopicModal() {
    $('topicNameInput').value = '';
    $('topicModal').classList.add('visible');
    $('topicNameInput').focus();
}

/**
 * Close the topic modal
 */
function closeTopicModal() {
    $('topicModal').classList.remove('visible');
}

/**
 * Save a new topic
 */
function saveTopic() {
    const name = $('topicNameInput').value.trim();
    if (!name) {
        alert('Enter a name');
        return;
    }

    State.data.topics.push({
        id: generateId(),
        name,
        subtopics: []
    });

    saveData();
    renderSidebar();
    closeTopicModal();
}

/**
 * Close the subtopic modal
 */
function closeSubtopicModal() {
    $('subtopicModal').classList.remove('visible');
}

/**
 * Save a new subtopic
 */
function saveSubtopic() {
    const name = $('subtopicNameInput').value.trim();
    if (!name) {
        alert('Enter a name');
        return;
    }

    const sub = {
        id: generateId(),
        name,
        nodes: []
    };

    currentTopicForSubtopic.subtopics.push(sub);
    saveData();
    renderSidebar();
    selectSubtopic(currentTopicForSubtopic, sub);
    closeSubtopicModal();
}

// ==================== SEARCH ====================

/**
 * Perform full-text search across all notes
 * @param {string} query - Search query
 */
function performSearch(query) {
    const results = $('searchResults');
    const topicsSection = $('topicsSection');

    // Clear search if empty query
    if (!query.trim()) {
        results.classList.remove('active');
        results.innerHTML = '';
        topicsSection.style.display = 'block';
        return;
    }

    const q = query.toLowerCase();
    const matches = [];

    // Search through all topics, subtopics, and nodes
    State.data.topics.forEach(topic => {
        topic.subtopics.forEach(sub => {
            sub.nodes.forEach(node => {
                const titleMatch = node.title?.toLowerCase().includes(q);
                const contentMatch = node.content?.toLowerCase().includes(q);

                if (titleMatch || contentMatch) {
                    let snippet = '';
                    if (contentMatch) {
                        const idx = node.content.toLowerCase().indexOf(q);
                        const start = Math.max(0, idx - 30);
                        const end = Math.min(node.content.length, idx + q.length + 30);
                        const rawSnippet = (start > 0 ? '...' : '') +
                            node.content.substring(start, end) +
                            (end < node.content.length ? '...' : '');
                        // Escape HTML and highlight match
                        snippet = escapeHtml(rawSnippet).replace(
                            new RegExp(escapeHtml(q), 'gi'),
                            '<mark>$&</mark>'
                        );
                    }
                    matches.push({ node, topic, sub, snippet });
                }
            });
        });
    });

    // Hide topics section and show results
    topicsSection.style.display = 'none';
    results.classList.add('active');

    if (matches.length === 0) {
        results.innerHTML = '<div style="padding:15px;color:var(--text2);font-size:13px;">No results found</div>';
        return;
    }

    // Render results (limit to 20)
    results.innerHTML = matches.slice(0, 20).map(m => `
        <div class="search-result" onclick="goToSearchResult('${m.topic.id}', '${m.sub.id}', '${m.node.id}')">
            <div class="title">${escapeHtml(m.node.title)}</div>
            <div class="path">${escapeHtml(m.topic.name)} → ${escapeHtml(m.sub.name)}</div>
            ${m.snippet ? `<div class="snippet">${m.snippet}</div>` : ''}
        </div>
    `).join('');
}

/**
 * Navigate to a search result
 * @param {string} topicId - Topic ID
 * @param {string} subId - Subtopic ID
 * @param {string} nodeId - Node ID
 */
function goToSearchResult(topicId, subId, nodeId) {
    const topic = State.data.topics.find(t => t.id === topicId);
    const sub = topic?.subtopics.find(s => s.id === subId);

    if (sub) {
        // Clear search
        $('searchInput').value = '';
        $('searchResults').classList.remove('active');
        $('topicsSection').style.display = 'block';

        // Navigate
        switchView('notes');
        selectSubtopic(topic, sub);

        // Select the node after a short delay
        setTimeout(() => {
            const node = sub.nodes.find(n => n.id === nodeId);
            if (node) selectNode(node);
        }, 100);
    }
}

// ==================== SIDEBAR TOGGLE ====================

/**
 * Toggle sidebar visibility
 */
function toggleSidebar() {
    const sidebar = $('sidebar');
    const toggle = $('sidebarToggle');
    sidebar.classList.toggle('collapsed');
    toggle.textContent = sidebar.classList.contains('collapsed') ? '▶' : '◀';
}
