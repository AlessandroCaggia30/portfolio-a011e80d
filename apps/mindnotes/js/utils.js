/**
 * UTILS.JS - Utility Functions
 *
 * This module provides common utility functions used across the application.
 * Includes security helpers, DOM utilities, and formatting functions.
 */

// ==================== SECURITY ====================

/**
 * Escape HTML to prevent XSS attacks
 * @param {string} text - Raw text to escape
 * @returns {string} HTML-safe text
 */
function escapeHtml(text) {
    const div = document.createElement('div');
    div.textContent = text;
    return div.innerHTML;
}

// ==================== UI FEEDBACK ====================

/**
 * Show a toast notification
 * @param {string} message - Message to display
 * @param {number} duration - Duration in ms (default: 2500)
 */
function showToast(message, duration = 2500) {
    const toast = document.createElement('div');
    toast.className = 'toast';
    toast.textContent = message;
    document.body.appendChild(toast);

    // Trigger animation
    setTimeout(() => toast.classList.add('show'), 10);

    // Remove after duration
    setTimeout(() => {
        toast.classList.remove('show');
        setTimeout(() => toast.remove(), 300);
    }, duration);
}

// ==================== DOM UTILITIES ====================

/**
 * Get element by ID with type safety
 * @param {string} id - Element ID
 * @returns {HTMLElement|null}
 */
function $(id) {
    return document.getElementById(id);
}

/**
 * Query selector shorthand
 * @param {string} selector - CSS selector
 * @param {HTMLElement} context - Context element (default: document)
 * @returns {HTMLElement|null}
 */
function $$(selector, context = document) {
    return context.querySelector(selector);
}

/**
 * Query selector all shorthand
 * @param {string} selector - CSS selector
 * @param {HTMLElement} context - Context element (default: document)
 * @returns {NodeList}
 */
function $$$(selector, context = document) {
    return context.querySelectorAll(selector);
}

// ==================== CONTENT FORMATTING ====================

/**
 * Format markdown-like content to HTML
 * Preserves LaTeX blocks and applies formatting
 * @param {string} text - Raw content with markdown
 * @returns {string} Formatted HTML
 */
function formatContent(text) {
    // Store LaTeX blocks to preserve them
    const latexBlocks = [];

    let processed = text
        // Preserve display math $$...$$
        .replace(/\$\$([\s\S]*?)\$\$/g, (match) => {
            latexBlocks.push(match);
            return `%%LATEX${latexBlocks.length - 1}%%`;
        })
        // Preserve inline math $...$
        .replace(/\$([^\$]+?)\$/g, (match) => {
            latexBlocks.push(match);
            return `%%LATEX${latexBlocks.length - 1}%%`;
        });

    // Escape HTML first for security, then apply formatting
    processed = escapeHtml(processed)
        // Highlights
        .replace(/==(.+?)==/g, '<span class="hl-yellow">$1</span>')
        .replace(/::(.+?)::/g, '<span class="hl-green">$1</span>')
        .replace(/~~(.+?)~~/g, '<span class="hl-pink">$1</span>')
        // Text formatting
        .replace(/\*\*(.+?)\*\*/g, '<strong>$1</strong>')
        .replace(/\*(.+?)\*/g, '<em>$1</em>')
        .replace(/`([^`]+?)`/g, '<code>$1</code>')
        // Block elements
        .replace(/^&gt; (.+)$/gm, '<blockquote>$1</blockquote>')
        .replace(/^- (.+)$/gm, '<li>$1</li>')
        .replace(/^(\d+)\. (.+)$/gm, '<li>$2</li>')
        // Headings
        .replace(/^### (.+)$/gm, '<h4>$1</h4>')
        .replace(/^## (.+)$/gm, '<h3>$1</h3>')
        .replace(/^# (.+)$/gm, '<h2>$1</h2>')
        // Line breaks
        .replace(/\n/g, '<br>');

    // Restore LaTeX blocks
    latexBlocks.forEach((block, i) => {
        processed = processed.replace(`%%LATEX${i}%%`, block);
    });

    return processed;
}

/**
 * Render LaTeX math in an element using KaTeX
 * @param {HTMLElement} element - Element containing LaTeX
 */
function renderMath(element) {
    if (typeof renderMathInElement !== 'undefined') {
        renderMathInElement(element, {
            delimiters: [
                { left: '$$', right: '$$', display: true },
                { left: '$', right: '$', display: false }
            ],
            throwOnError: false
        });
    }
}

// ==================== CLIPBOARD ====================

/**
 * Copy text to clipboard using modern API with fallback
 * @param {string} text - Text to copy
 * @returns {Promise<boolean>} Success status
 */
async function copyToClipboard(text) {
    try {
        await navigator.clipboard.writeText(text);
        return true;
    } catch (e) {
        // Fallback for older browsers
        const textarea = document.createElement('textarea');
        textarea.value = text;
        textarea.style.position = 'fixed';
        textarea.style.opacity = '0';
        document.body.appendChild(textarea);
        textarea.select();
        const success = document.execCommand('copy');
        document.body.removeChild(textarea);
        return success;
    }
}

// ==================== COLOR DEFINITIONS ====================

/**
 * Node color palette
 */
const NODE_COLORS = {
    blue: '#74b9ff',
    purple: '#a29bfe',
    green: '#55efc4',
    orange: '#ffeaa7',
    red: '#ff7675',
    pink: '#fd79a8',
    teal: '#00cec9',
    yellow: '#ffd93d',
    coral: '#ff6b6b',
    lavender: '#b8a9c9',
    mint: '#a8e6cf',
    peach: '#ffb4a2'
};
