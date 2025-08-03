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

Reference cells from named tables using multiple formats:

```org
#+NAME: Budget
| Employee | Salary   | FICA    |
|----------|----------|---------|  
| Alice    | 100,000  | =B2*0.0765 |
| Bob      | 80,000   | =B3*0.0765 |
| TOTALS   | =SUM(B2:B3) | =SUM(C2:C3) |

Total FICA Cost = Budget.TOTALS[1]     # Reference TOTALS row, column 1
Alice's Salary = Budget.B2             # Direct cell reference
Total Payroll = Budget[4,1]            # Row 4, column 1 format
```


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

## License

GPL-3.0-or-later
