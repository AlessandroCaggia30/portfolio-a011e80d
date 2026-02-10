/**
 * VIEWS.JS - View Switching & Theme
 *
 * This module handles switching between different views
 * (Notes, Flashcards, Quiz) and theme management.
 */

// ==================== VIEW SWITCHING ====================

/**
 * Switch between application views
 * @param {string} view - View name ('notes', 'flashcards', 'quiz')
 */
function switchView(view) {
    State.currentView = view;

    // Update tab buttons
    document.querySelectorAll('.nav-tab').forEach(t => t.classList.remove('active'));
    $('tab' + view.charAt(0).toUpperCase() + view.slice(1)).classList.add('active');

    // Update view visibility
    $('canvasWrapper').style.display = view === 'notes' ? 'flex' : 'none';
    $('detailPanel').style.display = view === 'notes' ? 'flex' : 'none';
    $('flashcardsView').classList.toggle('active', view === 'flashcards');
    $('quizView').classList.toggle('active', view === 'quiz');

    // Update sidebar visibility
    $('topicsSection').style.display = view === 'notes' ? 'block' : 'none';
    document.querySelector('.add-btn').style.display = view === 'notes' ? 'block' : 'none';

    // Load view-specific data
    if (view === 'flashcards') updateFlashcardStats();
    if (view === 'quiz') updateQuizView();
}

// ==================== FLASHCARDS VIEW ====================

/**
 * Update flashcard statistics display
 */
function updateFlashcardStats() {
    let totalCards = 0;
    let topicsWithCards = 0;

    State.data.topics.forEach(topic => {
        let topicHasCards = false;
        topic.subtopics.forEach(sub => {
            const fcCount = sub.nodes.filter(n => n.flashcard).length;
            totalCards += fcCount;
            if (fcCount > 0) topicHasCards = true;
        });
        if (topicHasCards) topicsWithCards++;
    });

    $('fcTotalCards').textContent = totalCards;
    $('fcTotalTopics').textContent = topicsWithCards;
    $('startFcBtn').disabled = totalCards === 0;

    // Update badge
    const badge = $('fcBadge');
    if (totalCards > 0) {
        badge.textContent = totalCards;
        badge.style.display = 'inline';
    } else {
        badge.style.display = 'none';
    }
}

/**
 * Start flashcard review session
 */
function startFlashcards() {
    alert('Flashcard review app coming soon! You have ' + $('fcTotalCards').textContent + ' cards ready.');
}

// ==================== QUIZ VIEW ====================

/**
 * Update quiz view display
 */
function updateQuizView() {
    let totalNotes = 0;
    State.data.topics.forEach(topic => {
        topic.subtopics.forEach(sub => {
            totalNotes += sub.nodes.length;
        });
    });
    $('quizTotalNotes').textContent = totalNotes;

    // Render topic selection buttons
    const container = $('quizTopics');
    container.innerHTML = '';

    State.data.topics.forEach(topic => {
        topic.subtopics.forEach(sub => {
            if (sub.nodes.length === 0) return;

            const btn = document.createElement('button');
            btn.className = 'quiz-topic-btn' + (State.selectedQuizTopics.has(sub.id) ? ' selected' : '');
            btn.textContent = `${topic.name} → ${sub.name} (${sub.nodes.length})`;
            btn.onclick = () => {
                if (State.selectedQuizTopics.has(sub.id)) {
                    State.selectedQuizTopics.delete(sub.id);
                } else {
                    State.selectedQuizTopics.add(sub.id);
                }
                updateQuizView();
            };
            container.appendChild(btn);
        });
    });
}

/**
 * Export notes for quiz generation
 */
function exportForQuiz() {
    let exportText = '# MY STUDY NOTES\n\n';
    exportText += 'Please create an advanced quiz based on these notes. Include:\n';
    exportText += '- Multiple choice questions\n';
    exportText += '- True/False questions\n';
    exportText += '- Short answer questions\n';
    exportText += '- Problem-solving questions\n\n';
    exportText += '---\n\n';

    const topicsToExport = State.selectedQuizTopics.size > 0 ? State.selectedQuizTopics : null;

    State.data.topics.forEach(topic => {
        topic.subtopics.forEach(sub => {
            if (topicsToExport && !topicsToExport.has(sub.id)) return;
            if (sub.nodes.length === 0) return;

            exportText += `## ${topic.name} - ${sub.name}\n\n`;
            sub.nodes.forEach(node => {
                exportText += `### ${node.title}\n`;
                exportText += node.content + '\n\n';
            });
        });
    });

    $('exportText').value = exportText;
    $('exportBox').style.display = 'block';
}

/**
 * Copy exported notes to clipboard
 */
async function copyExport() {
    const text = $('exportText').value;
    const success = await copyToClipboard(text);
    showToast(success ? 'Notes copied to clipboard!' : 'Failed to copy');
}

// ==================== THEME ====================

/**
 * Toggle between light and dark themes
 */
function toggleTheme() {
    const html = document.documentElement;
    const currentTheme = html.getAttribute('data-theme');
    const newTheme = currentTheme === 'light' ? null : 'light';

    if (newTheme) {
        html.setAttribute('data-theme', newTheme);
    } else {
        html.removeAttribute('data-theme');
    }

    localStorage.setItem('mindnotes_theme', newTheme || 'dark');

    // Update button icon
    $('themeToggle').textContent = newTheme === 'light' ? '☀️' : '🌙';
}

/**
 * Load saved theme from localStorage
 */
function loadTheme() {
    const savedTheme = localStorage.getItem('mindnotes_theme') || 'dark';

    if (savedTheme === 'light') {
        document.documentElement.setAttribute('data-theme', 'light');
    } else {
        document.documentElement.removeAttribute('data-theme');
    }

    $('themeToggle').textContent = savedTheme === 'light' ? '☀️' : '🌙';
}
