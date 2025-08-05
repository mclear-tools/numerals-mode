# Numerals Mode for Emacs

A minor mode that provides literate calculation functionality similar to Obsidian's Numerals plugin.

## Features

- **Variable Assignment**: Define variables with natural syntax like `Monthly Rent = 2350.00`
- **Automatic Updates**: Calculations update when you save the file or manually trigger recalculation
- **Variable References**: Use previously defined variables in calculations
- **Table Calculations**: Full support for Excel-style formulas in markdown and org-mode tables
- **Cross-Table References**: Reference cells from named tables (e.g., `Budget.TOTALS[0]`)
- **Overlay Display**: Results appear as non-intrusive overlays without modifying your file
- **Natural Syntax**: Support for variable names with spaces and comma-formatted numbers
- **Dependency Resolution**: Smart 4-pass system ensures correct calculation order
- **Excel Functions**: SUM, AVERAGE, COUNT, MAX, MIN with cell ranges support

## Installation

1. Clone this repository or download the files
2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/numerals-mode")
(require 'numerals-mode)
```

## Usage

Enable numerals-mode in any buffer:

```
M-x numerals-mode
```

### Org-Mode Integration

Numerals mode integrates seamlessly with org-mode files. You can automatically activate numerals-mode in org files using:

1. **Org Keywords**: Add `#+STARTUP: numerals` or `#+PROPERTY: numerals-mode t` to your org file
2. **File-Local Variables**: Add `# -*- eval: (numerals-mode 1) -*-` to the first line

When numerals-mode is active in an org-mode buffer:
- `org-pretty-entities` is automatically disabled to prevent conflicts
- Original settings are restored when numerals-mode is disabled

## Commands

When numerals-mode is active, the following commands are available:

| Key Binding | Command | Description |
|-------------|---------|-------------|
| `C-c C-c` | `numerals-recalculate` | Manually recalculate all expressions |
| `C-c C-k` | `numerals-clear` | Clear all variables and calculations |
| `C-c C-t` | `numerals-toggle-overlays` | Toggle visibility of all overlays in buffer |
| `C-c C-l` | `numerals-toggle-overlay-at-point` | Toggle overlay on current line |

## Examples

Then start writing calculations. Results will update when you save the file:

```
Monthly Rent = 2350.00              => Monthly Rent: 2350
Taxes = 3560.22                     => Taxes: 3560.22
Insurance = 2000.00                 => Insurance: 2000
Total Expenses = Taxes + Insurance  => Total Expenses: 5560.22
Yearly Profit = (Monthly Rent * 12) - Total Expenses => Yearly Profit: 22639.78
```

### Table Calculations

Numerals mode supports calculations in both markdown and org-mode tables:

```markdown
| Product | Price | Quantity | Total       |
|---------|-------|----------|-------------|
| Apple   | 2.50  | 10       | =B2*C2      |
| Orange  | 3.00  | 5        | =B3*C3      |
| Total   |       |          | =SUM(D2:D3) |
```

Supported functions: `SUM`, `AVERAGE`, `COUNT`, `MAX`, `MIN`

### Cross-Table References

Reference cells from named tables using `TableName.CellRef` syntax:

```org
#+NAME: Sales
| Product | 2023    | 2024    |
|---------|---------|---------|
| Gas     | 1,572,921 | 1,561,805 |
| Lottery | 826,356   | 743,353   |

#+NAME: Summary  
| Category    | Formula                    | Result    |
|-------------|----------------------------|-----------|
| Total 2023  | =Sales.B2+Sales.B3         | 2,399,277 |
| Total 2024  | =Sales.C2+Sales.C3         | 2,305,158 |
| Difference  | =B3-B2                     | -94,119   |

# Variables can also reference table cells
Gas_Growth = (Sales.C2 / Sales.B2 - 1) * 100  # => -0.70%
```

**Supported reference formats:**
- `TableName.B2` - Direct cell reference (most common)
- `TableName.TOTALS[0]` - Reference calculated values in TOTALS rows
- Works in both table formulas and variable assignments
- Handles comma-formatted numbers correctly
- Supports compound operations: `=Table1.B2+Table2.C3*Table3.D4`


## Examples

Check out the example files:
- `examples/property-analysis.txt` - Property investment calculations with variables
- `examples/table-calculations.txt` - Table calculation examples for both org-mode and markdown

## Requirements

- Emacs 26.1 or later (for built-in calc support)

## Technical Details

### Dependency Resolution

Numerals-mode uses a sophisticated 4-pass dependency resolution system:
1. **Pass 1**: Simple variables (literals and basic math)
2. **Pass 2**: Tables (can reference simple variables)
3. **Pass 3**: Complex variables (can reference tables and other variables)
4. **Pass 4**: Table reprocessing (for formulas referencing complex variables)

This ensures that calculations always occur in the correct order, preventing "undefined variable" errors.

### Performance

- Overlay pooling for efficient memory usage
- Position tracking to prevent duplicate processing
- Selective reprocessing of only changed elements

### Export Integration

Numerals-mode automatically integrates with org-mode's export system to substitute calculated results in exported documents:

#### Export Behavior

- **Simple literal assignments** (e.g., `Monthly Rent = 2350.00`) - exported as-is without overlay duplication
- **Complex expressions** (e.g., `Total = Rent + Taxes`) - exported with calculated results: `Total = Rent + Taxes => 4,350.5`
- **Table formulas** (e.g., `=SUM(B2:B5)`) - replaced entirely with calculated values: `32,604`
- **Cross-table references** (e.g., `Budget.TOTALS[0]`) - exported with resolved values

#### Supported Export Backends

- HTML (`C-c C-e h h`)
- LaTeX/PDF (`C-c C-e l l`, `C-c C-e l p`)
- All other org-mode export backends

#### Export Commands

- `M-x numerals-export-toggle-substitution` - Toggle overlay substitution during export
- `M-x numerals-export-preview-substitutions` - Preview which overlays will be substituted

Export integration is automatically enabled when numerals-mode is active.

### Pandoc Export

Numerals-mode provides special support for exporting documents via pandoc, preserving all calculated values:

#### Basic Usage

```elisp
;; Export current buffer to HTML
M-x numerals-pandoc-export-to-html

;; Export to PDF (requires LaTeX)
M-x numerals-pandoc-export-to-pdf

;; Export to Word document
M-x numerals-pandoc-export-to-docx

;; Export with custom format
M-x numerals-pandoc-export
```

#### Configuration

```elisp
;; Set default output format
(setq numerals-pandoc-output-format "html")

;; Add custom pandoc arguments
(setq numerals-pandoc-extra-args '("--toc" "--standalone" "--number-sections"))

;; Keep temporary preprocessed files for debugging
(setq numerals-pandoc-delete-temp-file nil)
```

#### How It Works

1. Creates a temporary copy of your buffer
2. Substitutes all numerals overlays with their calculated values
3. Runs pandoc on the preprocessed file
4. Outputs the final document with all calculations intact

#### Advanced Features

- **Preview preprocessed content**: `M-x numerals-pandoc-preview-preprocessed`
- **Export region only**: `M-x numerals-pandoc-export-region`
- **Shell wrapper script**: `M-x numerals-pandoc-create-shell-wrapper`

The shell wrapper allows command-line usage:
```bash
./numerals-pandoc.sh input.org output.html --toc --standalone
```

#### Supported Formats

All pandoc output formats are supported, including:
- HTML (with or without standalone wrapper)
- PDF (via LaTeX)
- Microsoft Word (.docx)
- Markdown variants
- LaTeX
- EPUB
- And many more

## License

GPL-3.0-or-later
