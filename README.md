# Numerals Mode for Emacs

A minor mode that provides literate calculation functionality similar to Obsidian's Numerals plugin.

## Features

- **Variable Assignment**: Define variables with natural syntax like `Monthly Rent = 2350.00`
- **Live Calculations**: Expressions are evaluated automatically as you type
- **Variable References**: Use previously defined variables in calculations
- **Overlay Display**: Results appear as overlays, not modifying your file
- **Natural Syntax**: Support for variable names with spaces

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

Then start writing calculations:

```
Monthly Rent = 2350.00              => Monthly Rent: 2350
Taxes = 3560.22                     => Taxes: 3560.22
Insurance = 2000.00                 => Insurance: 2000
Total Expenses = Taxes + Insurance  => Total Expenses: 5560.22
Yearly Profit = (Monthly Rent * 12) - Total Expenses => Yearly Profit: 22639.78
```

### Commands

- `numerals-mode`: Toggle numerals mode
- `numerals-recalculate`: Manually recalculate all expressions
- `numerals-clear`: Clear all variables and calculations

## Examples

See the `examples/property-analysis.txt` file for a complete example of property investment calculations.

## Requirements

- Emacs 26.1 or later (for built-in calc support)

## License

GPL-3.0-or-later