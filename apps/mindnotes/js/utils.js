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

// ==================== LATEX HELPERS ====================

/**
 * Normalize LaTeX display/inline delimiters to $$/$ form
 * Converts \[...\] → $$...$$ and \(...\) → $...$
 * @param {string} text - Raw text with LaTeX delimiters
 * @returns {string} Text with normalized delimiters
 */
function normalizeLatexDelimiters(text) {
    // \[...\] → $$...$$ (display math)
    text = text.replace(/\\\[([\s\S]*?)\\\]/g, (m, inner) => `$$${inner}$$`);
    // \(...\) → $...$ (inline math)
    text = text.replace(/\\\(([\s\S]*?)\\\)/g, (m, inner) => `$${inner}$`);
    return text;
}

/**
 * Convert common LaTeX structural commands to markdown/HTML equivalents.
 * Call AFTER math blocks have been extracted to placeholders.
 * Color commands (\textcolor, \colorbox) are converted to %%TCLR%% markers
 * that must be restored later with restoreColorMarkers() after HTML escaping.
 * @param {string} text - Text with math already extracted
 * @returns {string} Text with LaTeX commands converted to markdown
 */
function processLatexCommands(text) {
    if (!text) return text;
    if (!text.includes('\\')) return text;

    // Color commands → markers FIRST (before other commands to avoid nested brace issues)
    // Handles one level of nested braces in content: \textcolor{blue}{\textbf{text}}
    text = text.replace(/\\textcolor\{([^}]*)\}\{((?:[^{}]|\{[^{}]*\})*)\}/g,
        (m, color, content) => `%%TCLR:${color}%%${content}%%/TCLR%%`);
    text = text.replace(/\\colorbox\{([^}]*)\}\{((?:[^{}]|\{[^{}]*\})*)\}/g,
        (m, color, content) => `%%CBOX:${color}%%${content}%%/CBOX%%`);

    // Environments
    text = text.replace(/\\begin\{(itemize|enumerate|description)\}\s*/g, '\n');
    text = text.replace(/\\end\{(itemize|enumerate|description)\}\s*/g, '\n');
    text = text.replace(/\\begin\{(center|quote|quotation|abstract|document|figure|table|verbatim)\}\s*/g, '\n');
    text = text.replace(/\\end\{(center|quote|quotation|abstract|document|figure|table|verbatim)\}\s*/g, '\n');

    // Theorem-like environments
    text = text.replace(/\\begin\{(theorem|lemma|proposition|corollary|definition|remark|example|assumption)\}\s*/g,
        (m, env) => `\n**${env.charAt(0).toUpperCase() + env.slice(1)}.** `);
    text = text.replace(/\\end\{(theorem|lemma|proposition|corollary|definition|remark|example|assumption)\}\s*/g, '\n');
    text = text.replace(/\\begin\{proof\}\s*/g, '\n*Proof.* ');
    text = text.replace(/\\end\{proof\}\s*/g, ' ◻\n');

    // List items
    text = text.replace(/[ \t]*\\item\[([^\]]*)\]\s*/g, '\n- $1 ');
    text = text.replace(/[ \t]*\\item\s*/g, '\n- ');

    // Sections → markdown headings
    text = text.replace(/\\section\*?\{([^}]*)\}/g, '\n# $1\n');
    text = text.replace(/\\subsection\*?\{([^}]*)\}/g, '\n## $1\n');
    text = text.replace(/\\subsubsection\*?\{([^}]*)\}/g, '\n### $1\n');
    text = text.replace(/\\paragraph\*?\{([^}]*)\}/g, '\n**$1** ');

    // Text formatting
    text = text.replace(/\\textbf\{([^}]*)\}/g, '**$1**');
    text = text.replace(/\\textit\{([^}]*)\}/g, '*$1*');
    text = text.replace(/\\emph\{([^}]*)\}/g, '*$1*');
    text = text.replace(/\\texttt\{([^}]*)\}/g, '`$1`');
    text = text.replace(/\\underline\{([^}]*)\}/g, '$1');
    text = text.replace(/\\textsc\{([^}]*)\}/g, '$1');

    // URLs and links
    text = text.replace(/\\href\{([^}]*)\}\{([^}]*)\}/g, '$2 ($1)');
    text = text.replace(/\\url\{([^}]*)\}/g, '$1');

    // Line breaks and spacing
    text = text.replace(/\\\\/g, '\n');
    text = text.replace(/\\bigskip\s*/g, '\n\n');
    text = text.replace(/\\medskip\s*/g, '\n');
    text = text.replace(/\\smallskip\s*/g, '\n');
    text = text.replace(/\\[hv]space\*?\{[^}]*\}/g, ' ');
    text = text.replace(/\\noindent\s*/g, '');
    text = text.replace(/\\par\b\s*/g, '\n\n');
    text = text.replace(/\\newline\s*/g, '\n');
    text = text.replace(/\\newpage\s*/g, '\n');
    text = text.replace(/\\clearpage\s*/g, '\n');

    // Misc commands
    text = text.replace(/\\centering\s*/g, '');
    text = text.replace(/\\maketitle\s*/g, '');
    text = text.replace(/\\tableofcontents\s*/g, '');
    text = text.replace(/\\footnote\{([^}]*)\}/g, ' ($1)');
    text = text.replace(/\\label\{[^}]*\}/g, '');
    text = text.replace(/\\ref\{([^}]*)\}/g, '[$1]');
    text = text.replace(/\\cite\{([^}]*)\}/g, '[$1]');

    // Clean up multiple blank lines
    text = text.replace(/\n{3,}/g, '\n\n');

    return text;
}

/**
 * Restore color markers (%%TCLR%%, %%CBOX%%) to HTML span elements.
 * Call AFTER escapeHtml and markdown formatting.
 */
function restoreColorMarkers(text) {
    text = text.replace(/%%TCLR:([^%]*)%%/g, '<span style="color:$1">');
    text = text.replace(/%%\/TCLR%%/g, '</span>');
    text = text.replace(/%%CBOX:([^%]*)%%/g, '<span style="background-color:$1;padding:2px 4px;border-radius:2px">');
    text = text.replace(/%%\/CBOX%%/g, '</span>');
    return text;
}

// ==================== CONTENT FORMATTING ====================

/**
 * Format markdown-like content to HTML
 * Preserves LaTeX blocks and applies formatting
 * @param {string} text - Raw content with markdown
 * @returns {string} Formatted HTML
 */
function formatContent(text) {
    // Normalize LaTeX delimiters (\[...\] → $$...$$, \(...\) → $...$)
    text = normalizeLatexDelimiters(text);

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

    // Process LaTeX structural commands (after math is safely extracted)
    processed = processLatexCommands(processed);

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

    // Restore color markers → HTML spans (after escaping so they aren't escaped)
    processed = restoreColorMarkers(processed);

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
                { left: '\\[', right: '\\]', display: true },
                { left: '$', right: '$', display: false },
                { left: '\\(', right: '\\)', display: false }
            ],
            throwOnError: false,
            macros: {
                "\\R": "\\mathbb{R}",
                "\\N": "\\mathbb{N}",
                "\\Z": "\\mathbb{Z}",
                "\\Q": "\\mathbb{Q}",
                "\\C": "\\mathbb{C}",
                "\\F": "\\mathbb{F}",
                "\\E": "\\mathbb{E}",
                "\\P": "\\mathbb{P}",
                "\\1": "\\mathbb{1}",
                "\\eps": "\\varepsilon",
                "\\Var": "\\operatorname{Var}",
                "\\Cov": "\\operatorname{Cov}",
                "\\Corr": "\\operatorname{Corr}",
                "\\rank": "\\operatorname{rank}",
                "\\tr": "\\operatorname{tr}",
                "\\diag": "\\operatorname{diag}",
                "\\supp": "\\operatorname{supp}",
                "\\sgn": "\\operatorname{sgn}",
                "\\argmax": "\\operatorname*{arg\\,max}",
                "\\argmin": "\\operatorname*{arg\\,min}",
                "\\plim": "\\operatorname*{plim}",
                "\\d": "\\mathrm{d}",
                "\\norm": "\\left\\|#1\\right\\|",
                "\\abs": "\\left|#1\\right|",
                "\\inner": "\\langle #1, #2 \\rangle",
                "\\floor": "\\left\\lfloor #1 \\right\\rfloor",
                "\\ceil": "\\left\\lceil #1 \\right\\rceil",
                "\\ind": "\\mathbb{1}"
            }
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
