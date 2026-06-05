"""Shared plot-style settings — exceptional, consistent look across all PNGs."""
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

# Palette — navy/yellow/amber + neutrals, paired with the hyper-table view.
PALETTE = {
    "primary":   "#1b3a64",   # deep navy
    "accent":    "#d4b800",   # warm yellow
    "secondary": "#5fa8d3",   # sky blue
    "neutral":   "#4a4a5a",   # slate
    "warn":      "#e74c3c",   # red
    "ok":        "#2c7a4b",   # forest green
    "muted":     "#8a8d9f",
    "bg":        "#ffffff",
    "grid":      "#e1e3eb",
}

# Cycle for multi-series plots
PALETTE_CYCLE = ["#1b3a64", "#d4b800", "#5fa8d3", "#e74c3c",
                 "#2c7a4b", "#9b59b6", "#e67e22", "#34495e"]


def apply_style():
    plt.rcParams.update({
        # Typography
        "font.family":      ["Helvetica Neue", "Arial", "sans-serif"],
        "font.size":         12,
        "axes.titlesize":    13.5,
        "axes.titleweight":  "bold",
        "axes.labelsize":    12,
        "xtick.labelsize":   11,
        "ytick.labelsize":   11,
        "legend.fontsize":   11,
        "legend.title_fontsize": 11.5,

        # Color/aesthetic defaults
        "axes.facecolor":    "#ffffff",
        "figure.facecolor":  "#ffffff",
        "savefig.facecolor": "#ffffff",
        "axes.edgecolor":    "#2a3142",
        "axes.linewidth":    1.1,
        "axes.titlepad":     12,
        "axes.labelpad":     8,
        "axes.spines.top":   False,
        "axes.spines.right": False,

        # Grid
        "axes.grid":         True,
        "grid.color":        PALETTE["grid"],
        "grid.linewidth":    0.7,
        "grid.alpha":        0.85,
        "grid.linestyle":    "-",
        "axes.axisbelow":    True,

        # Lines / markers
        "axes.prop_cycle":   plt.cycler("color", PALETTE_CYCLE),
        "lines.linewidth":   2.0,
        "lines.markersize":  6,

        # Figures
        "figure.dpi":        110,
        "savefig.dpi":       140,
        "savefig.bbox":      "tight",
        "savefig.pad_inches": 0.25,
        "figure.autolayout": False,
    })
