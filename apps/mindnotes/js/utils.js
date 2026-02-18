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

    // Theorem-like environments (with optional argument [title])
    text = text.replace(/\\begin\{(theorem|lemma|proposition|corollary|definition|remark|example|assumption|conjecture|axiom|notation|exercise|problem|solution|claim|fact|observation)\}\[([^\]]*)\]\s*/g,
        (m, env, title) => `\n**${env.charAt(0).toUpperCase() + env.slice(1)} (${title}).** `);
    text = text.replace(/\\begin\{(theorem|lemma|proposition|corollary|definition|remark|example|assumption|conjecture|axiom|notation|exercise|problem|solution|claim|fact|observation)\}\s*/g,
        (m, env) => `\n**${env.charAt(0).toUpperCase() + env.slice(1)}.** `);
    text = text.replace(/\\end\{(theorem|lemma|proposition|corollary|definition|remark|example|assumption|conjecture|axiom|notation|exercise|problem|solution|claim|fact|observation)\}\s*/g, '\n');
    text = text.replace(/\\begin\{proof\}\[([^\]]*)\]\s*/g, '\n*Proof ($1).* ');
    text = text.replace(/\\begin\{proof\}\s*/g, '\n*Proof.* ');
    text = text.replace(/\\end\{proof\}\s*/g, ' ◻\n');

    // Environments - remove begin/end markers
    text = text.replace(/\\begin\{(itemize|enumerate|description)\}(\[[^\]]*\])?\s*/g, '\n');
    text = text.replace(/\\end\{(itemize|enumerate|description)\}\s*/g, '\n');
    text = text.replace(/\\begin\{(center|quote|quotation|abstract|document|figure|table|verbatim|flushleft|flushright|minipage|multicols|wrapfigure)\}(\{[^}]*\})*\s*/g, '\n');
    text = text.replace(/\\end\{(center|quote|quotation|abstract|document|figure|table|verbatim|flushleft|flushright|minipage|multicols|wrapfigure)\}\s*/g, '\n');

    // Tabular → simple text table
    text = text.replace(/\\begin\{tabular\}\{[^}]*\}\s*/g, '\n');
    text = text.replace(/\\end\{tabular\}\s*/g, '\n');
    text = text.replace(/\\hline\s*/g, '');
    text = text.replace(/\\toprule\s*/g, '');
    text = text.replace(/\\midrule\s*/g, '');
    text = text.replace(/\\bottomrule\s*/g, '');
    text = text.replace(/\\cline\{[^}]*\}\s*/g, '');
    text = text.replace(/\\multicolumn\{[^}]*\}\{[^}]*\}\{([^}]*)\}/g, '$1');
    text = text.replace(/\\multirow\{[^}]*\}\{[^}]*\}\{([^}]*)\}/g, '$1');

    // Algorithm/pseudocode environments
    text = text.replace(/\\begin\{(algorithm|algorithmic|lstlisting|minted|verbatim)\}(\[[^\]]*\])?\s*/g, '\n```\n');
    text = text.replace(/\\end\{(algorithm|algorithmic|lstlisting|minted|verbatim)\}\s*/g, '\n```\n');
    text = text.replace(/\\caption\{([^}]*)\}/g, '\n**$1**\n');
    text = text.replace(/\\(State|Require|Ensure|Input|Output)\b\s*/g, '\n$1: ');
    text = text.replace(/\\(If|ElsIf|Else|EndIf|While|EndWhile|For|EndFor|ForAll|Return|Repeat|Until)\b\s*/g,
        (m, kw) => '\n' + kw + ' ');

    // TikZ/PGF → graceful fallback
    text = text.replace(/\\begin\{tikzpicture\}[\s\S]*?\\end\{tikzpicture\}/g, '\n[TikZ diagram]\n');
    text = text.replace(/\\begin\{pgfpicture\}[\s\S]*?\\end\{pgfpicture\}/g, '\n[PGF diagram]\n');

    // Equation environments → wrap in $$ for KaTeX
    text = text.replace(/\\begin\{(equation|displaymath)\}\*?\s*/g, '\n$$');
    text = text.replace(/\\end\{(equation|displaymath)\}\*?\s*/g, '$$\n');

    // Document preamble commands (strip them)
    text = text.replace(/\\documentclass(\[[^\]]*\])?\{[^}]*\}\s*/g, '');
    text = text.replace(/\\usepackage(\[[^\]]*\])?\{[^}]*\}\s*/g, '');
    text = text.replace(/\\newcommand\{[^}]*\}(\[[^\]]*\])?\{[^}]*\}\s*/g, '');
    text = text.replace(/\\renewcommand\{[^}]*\}(\[[^\]]*\])?\{[^}]*\}\s*/g, '');
    text = text.replace(/\\DeclareMathOperator\*?\{[^}]*\}\{[^}]*\}\s*/g, '');
    text = text.replace(/\\theoremstyle\{[^}]*\}\s*/g, '');
    text = text.replace(/\\newtheorem\{[^}]*\}(\[[^\]]*\])?\{[^}]*\}(\[[^\]]*\])?\s*/g, '');
    text = text.replace(/\\setlength\{[^}]*\}\{[^}]*\}\s*/g, '');
    text = text.replace(/\\pagestyle\{[^}]*\}\s*/g, '');
    text = text.replace(/\\thispagestyle\{[^}]*\}\s*/g, '');
    text = text.replace(/\\bibliographystyle\{[^}]*\}\s*/g, '');
    text = text.replace(/\\bibliography\{[^}]*\}\s*/g, '');

    // Title/author/date
    text = text.replace(/\\title\{([^}]*)\}/g, '\n# $1\n');
    text = text.replace(/\\author\{([^}]*)\}/g, '\n*$1*\n');
    text = text.replace(/\\date\{([^}]*)\}/g, '\n$1\n');

    // List items
    text = text.replace(/[ \t]*\\item\[([^\]]*)\]\s*/g, '\n- **$1** ');
    text = text.replace(/[ \t]*\\item\s*/g, '\n- ');

    // Sections → markdown headings
    text = text.replace(/\\chapter\*?\{([^}]*)\}/g, '\n# $1\n');
    text = text.replace(/\\section\*?\{([^}]*)\}/g, '\n# $1\n');
    text = text.replace(/\\subsection\*?\{([^}]*)\}/g, '\n## $1\n');
    text = text.replace(/\\subsubsection\*?\{([^}]*)\}/g, '\n### $1\n');
    text = text.replace(/\\paragraph\*?\{([^}]*)\}/g, '\n**$1** ');
    text = text.replace(/\\subparagraph\*?\{([^}]*)\}/g, '\n**$1** ');

    // Text formatting
    text = text.replace(/\\textbf\{([^}]*)\}/g, '**$1**');
    text = text.replace(/\\textit\{([^}]*)\}/g, '*$1*');
    text = text.replace(/\\emph\{([^}]*)\}/g, '*$1*');
    text = text.replace(/\\texttt\{([^}]*)\}/g, '`$1`');
    text = text.replace(/\\underline\{([^}]*)\}/g, '$1');
    text = text.replace(/\\textsc\{([^}]*)\}/g, '$1');
    text = text.replace(/\\textrm\{([^}]*)\}/g, '$1');
    text = text.replace(/\\textsf\{([^}]*)\}/g, '$1');
    text = text.replace(/\\mbox\{([^}]*)\}/g, '$1');
    text = text.replace(/\\fbox\{([^}]*)\}/g, '[$1]');

    // URLs and links
    text = text.replace(/\\href\{([^}]*)\}\{([^}]*)\}/g, '$2 ($1)');
    text = text.replace(/\\url\{([^}]*)\}/g, '$1');

    // Cross-references
    text = text.replace(/\\eqref\{([^}]*)\}/g, '([$1])');
    text = text.replace(/\\autoref\{([^}]*)\}/g, '[$1]');
    text = text.replace(/\\cref\{([^}]*)\}/g, '[$1]');
    text = text.replace(/\\nameref\{([^}]*)\}/g, '[$1]');
    text = text.replace(/\\pageref\{([^}]*)\}/g, '[p. $1]');

    // Line breaks and spacing
    text = text.replace(/\\\\/g, '\n');
    text = text.replace(/\\bigskip\s*/g, '\n\n');
    text = text.replace(/\\medskip\s*/g, '\n');
    text = text.replace(/\\smallskip\s*/g, '\n');
    text = text.replace(/\\[hv]space\*?\{[^}]*\}/g, ' ');
    text = text.replace(/\\noindent\s*/g, '');
    text = text.replace(/\\indent\s*/g, '');
    text = text.replace(/\\par\b\s*/g, '\n\n');
    text = text.replace(/\\newline\s*/g, '\n');
    text = text.replace(/\\newpage\s*/g, '\n');
    text = text.replace(/\\clearpage\s*/g, '\n');
    text = text.replace(/\\linebreak(\[\d\])?\s*/g, '\n');
    text = text.replace(/\\pagebreak(\[\d\])?\s*/g, '\n');
    text = text.replace(/\\allowbreak\s*/g, '');
    text = text.replace(/\\phantom\{[^}]*\}/g, ' ');
    text = text.replace(/\\hphantom\{[^}]*\}/g, ' ');
    text = text.replace(/\\vphantom\{[^}]*\}/g, '');
    text = text.replace(/~\s*/g, ' ');

    // Misc commands
    text = text.replace(/\\centering\s*/g, '');
    text = text.replace(/\\maketitle\s*/g, '');
    text = text.replace(/\\tableofcontents\s*/g, '');
    text = text.replace(/\\listoffigures\s*/g, '');
    text = text.replace(/\\listoftables\s*/g, '');
    text = text.replace(/\\appendix\s*/g, '\n# Appendix\n');
    text = text.replace(/\\footnote\{([^}]*)\}/g, ' ($1)');
    text = text.replace(/\\footnotetext\{([^}]*)\}/g, ' ($1)');
    text = text.replace(/\\footnotemark(\[\d+\])?\s*/g, '');
    text = text.replace(/\\label\{[^}]*\}/g, '');
    text = text.replace(/\\tag\{([^}]*)\}/g, '');
    text = text.replace(/\\ref\{([^}]*)\}/g, '[$1]');
    text = text.replace(/\\cite(\[[^\]]*\])?\{([^}]*)\}/g, '[$2]');
    text = text.replace(/\\nocite\{[^}]*\}/g, '');
    text = text.replace(/\\index\{[^}]*\}/g, '');
    text = text.replace(/\\thanks\{([^}]*)\}/g, ' ($1)');
    text = text.replace(/\\protect\s*/g, '');
    text = text.replace(/\\raggedright\s*/g, '');
    text = text.replace(/\\raggedleft\s*/g, '');
    text = text.replace(/\\sloppy\s*/g, '');
    text = text.replace(/\\frenchspacing\s*/g, '');

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
        // Horizontal rule
        .replace(/^---$/gm, '<hr>')
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
            strict: false,
            trust: true,
            macros: {
                // ===== Number sets =====
                "\\R": "\\mathbb{R}",
                "\\N": "\\mathbb{N}",
                "\\Z": "\\mathbb{Z}",
                "\\Q": "\\mathbb{Q}",
                "\\C": "\\mathbb{C}",
                "\\F": "\\mathbb{F}",
                "\\E": "\\mathbb{E}",
                "\\P": "\\mathbb{P}",
                "\\1": "\\mathbb{1}",
                "\\ind": "\\mathbb{1}",
                "\\Rn": "\\mathbb{R}^{#1}",
                // ===== Greek shortcuts =====
                "\\eps": "\\varepsilon",
                "\\vphi": "\\varphi",
                "\\vtheta": "\\vartheta",
                // ===== Operators (named) =====
                "\\Var": "\\operatorname{Var}",
                "\\Cov": "\\operatorname{Cov}",
                "\\Corr": "\\operatorname{Corr}",
                "\\MSE": "\\operatorname{MSE}",
                "\\Bias": "\\operatorname{Bias}",
                "\\rank": "\\operatorname{rank}",
                "\\tr": "\\operatorname{tr}",
                "\\diag": "\\operatorname{diag}",
                "\\supp": "\\operatorname{supp}",
                "\\sgn": "\\operatorname{sgn}",
                "\\im": "\\operatorname{im}",
                "\\ker": "\\operatorname{ker}",
                "\\dom": "\\operatorname{dom}",
                "\\ran": "\\operatorname{ran}",
                "\\span": "\\operatorname{span}",
                "\\proj": "\\operatorname{proj}",
                "\\grad": "\\operatorname{grad}",
                "\\curl": "\\operatorname{curl}",
                "\\divg": "\\operatorname{div}",
                "\\adj": "\\operatorname{adj}",
                "\\lcm": "\\operatorname{lcm}",
                "\\ord": "\\operatorname{ord}",
                "\\Aut": "\\operatorname{Aut}",
                "\\Hom": "\\operatorname{Hom}",
                "\\End": "\\operatorname{End}",
                "\\Id": "\\operatorname{Id}",
                "\\GL": "\\operatorname{GL}",
                "\\SL": "\\operatorname{SL}",
                "\\SO": "\\operatorname{SO}",
                "\\SU": "\\operatorname{SU}",
                // ===== Starred operators =====
                "\\argmax": "\\operatorname*{arg\\,max}",
                "\\argmin": "\\operatorname*{arg\\,min}",
                "\\plim": "\\operatorname*{plim}",
                "\\esssup": "\\operatorname*{ess\\,sup}",
                "\\essinf": "\\operatorname*{ess\\,inf}",
                // ===== Calculus =====
                "\\d": "\\mathrm{d}",
                "\\dd": "\\,\\mathrm{d}",
                "\\dv": "\\frac{\\mathrm{d} #1}{\\mathrm{d} #2}",
                "\\pdv": "\\frac{\\partial #1}{\\partial #2}",
                "\\pdvn": "\\frac{\\partial^{#1} #2}{\\partial #3^{#1}}",
                // ===== Delimiters & brackets =====
                "\\norm": "\\left\\|#1\\right\\|",
                "\\abs": "\\left|#1\\right|",
                "\\inner": "\\langle #1, #2 \\rangle",
                "\\floor": "\\left\\lfloor #1 \\right\\rfloor",
                "\\ceil": "\\left\\lceil #1 \\right\\rceil",
                "\\set": "\\left\\{#1\\right\\}",
                "\\paren": "\\left(#1\\right)",
                "\\brak": "\\left[#1\\right]",
                "\\ang": "\\left\\langle #1 \\right\\rangle",
                // ===== Probability & Statistics =====
                "\\Prob": "\\mathbb{P}\\left(#1\\right)",
                "\\Exp": "\\mathbb{E}\\left[#1\\right]",
                "\\given": "\\,\\middle|\\,",
                "\\iid": "\\overset{\\text{iid}}{\\sim}",
                "\\dto": "\\overset{d}{\\to}",
                "\\pto": "\\overset{p}{\\to}",
                "\\asto": "\\overset{a.s.}{\\to}",
                "\\Normal": "\\mathcal{N}",
                "\\Bernoulli": "\\operatorname{Bernoulli}",
                "\\Binomial": "\\operatorname{Bin}",
                "\\Poisson": "\\operatorname{Pois}",
                "\\Uniform": "\\operatorname{Unif}",
                "\\Exponential": "\\operatorname{Exp}",
                // ===== Economics =====
                "\\Lagr": "\\mathcal{L}",
                "\\Hamilt": "\\mathcal{H}",
                "\\Bellman": "\\mathcal{V}",
                "\\elast": "\\varepsilon",
                "\\utilfn": "U",
                "\\prodfn": "F",
                "\\profit": "\\Pi",
                "\\mc": "\\mathrm{MC}",
                "\\mr": "\\mathrm{MR}",
                "\\ac": "\\mathrm{AC}",
                "\\avc": "\\mathrm{AVC}",
                "\\gdp": "\\mathrm{GDP}",
                "\\gnp": "\\mathrm{GNP}",
                "\\cpi": "\\mathrm{CPI}",
                "\\tfp": "\\mathrm{TFP}",
                // ===== Arrows & relations =====
                "\\implies": "\\Longrightarrow",
                "\\iff": "\\Longleftrightarrow",
                "\\into": "\\hookrightarrow",
                "\\onto": "\\twoheadrightarrow",
                "\\iso": "\\cong",
                "\\defeq": "\\coloneqq",
                "\\eqdef": "\\eqqcolon",
                // ===== Text shortcuts =====
                "\\st": "\\text{ s.t. }",
                "\\where": "\\text{ where }",
                "\\with": "\\text{ with }",
                "\\and": "\\text{ and }",
                "\\orr": "\\text{ or }",
                "\\forallx": "\\text{ for all }",
                "\\iif": "\\text{ if }",
                "\\ow": "\\text{ otherwise}",
                // ===== Decorations =====
                "\\ol": "\\overline{#1}",
                "\\ul": "\\underline{#1}",
                "\\wh": "\\widehat{#1}",
                "\\wt": "\\widetilde{#1}",
                "\\ob": "\\overbrace{#1}",
                "\\ub": "\\underbrace{#1}",
                "\\cancel": "\\xcancel{#1}",
                "\\hl": "\\colorbox{yellow}{$#1$}",
                // ===== Vectors & matrices =====
                "\\vv": "\\mathbf{#1}",
                "\\mat": "\\mathbf{#1}",
                "\\T": "^{\\mathsf{T}}",
                "\\inv": "^{-1}",
                "\\pinv": "^{\\dagger}",
                // ===== Misc =====
                "\\qed": "\\blacksquare",
                "\\contra": "\\Rightarrow\\!\\Leftarrow",
                "\\diam": "\\operatorname{diam}",
                "\\dist": "\\operatorname{dist}",
                "\\vol": "\\operatorname{vol}",
                "\\Re": "\\operatorname{Re}",
                "\\Im": "\\operatorname{Im}"
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
