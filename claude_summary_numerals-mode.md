# Numerals-Mode for Emacs

## Project Overview

This project aims to recreate the variable assignment and calculation functionality of Obsidian's Numerals plugin in Emacs. The goal is to enable literate calculations with automatic variable references and live updates, similar to a lightweight spreadsheet embedded in text.

## Target User Experience

Users should be able to write calculations in plain text with natural syntax:

```
Monthly Rent = 2350.00              => Monthly Rent: 2350
Taxes = 3560.22                     => Taxes: 3560.22
Insurance = 2000.00                 => Insurance: 2000
Total Expenses = Taxes + Insurance  => Total Expenses: 5560.22
Yearly Profit = (Monthly Rent * 12) - Total Expenses => Yearly Profit: 22639.78
```

## Core Features Required

1. **Variable Assignment**: `Variable Name = expression` creates a named variable
2. **Variable References**: Variables can be used in subsequent calculations
3. **Live Updates**: Changing a variable value updates all dependent calculations
4. **Overlay Results**: Display results as overlays (not saved to file)
5. **Natural Syntax**: Support variable names with spaces, intuitive operators
6. **Auto-calculation**: Update results as user types (with debouncing)

## Technical Architecture

### Core Components

1. **Parser Module** (`numerals-parser.el`)
   - Regex patterns for variable assignments and calculations
   - Line-by-line parsing logic
   - Expression validation

2. **Calculator Module** (`numerals-calc.el`)
   - Integration with Emacs calc
   - Variable substitution in expressions
   - Error handling for invalid calculations

3. **Variable Management** (`numerals-variables.el`)
   - Storage of variable name/value pairs (buffer-local)
   - Dependency tracking between variables
   - Update propagation logic

4. **Display Module** (`numerals-display.el`)
   - Overlay creation and management
   - Result formatting
   - Visual feedback for errors

5. **Mode Definition** (`numerals-mode.el`)
   - Minor mode implementation
   - Auto-update timer management
   - Keybindings and user commands

### Syntax Specification

**Variable Assignment:**
- Pattern: `VARIABLE_NAME = EXPRESSION`
- Variable names: Letters, numbers, spaces allowed
- Examples: `Rent = 1200`, `Monthly Income = 5000`

**Calculations:**
- Pattern: `= EXPRESSION` or `VARIABLE = EXPRESSION`
- Support standard math operators: `+`, `-`, `*`, `/`, `^`, `()`
- Variable references by name

**Special Operators (Future):**
- `@sum` for auto-summing values above
- Range references like `A1:A5`

## Implementation Strategy

### Phase 1: Core Functionality
1. Basic variable assignment parsing
2. Simple arithmetic with Emacs calc
3. Overlay display system
4. Manual recalculation command

### Phase 2: Live Updates
1. Auto-update on buffer changes
2. Dependency tracking
3. Debounced recalculation
4. Error handling and display

### Phase 3: Enhanced Features
1. Support for variable names with spaces
2. Mathematical functions (sin, cos, sqrt, etc.)
3. Unit support (leveraging calc's capabilities)
4. Better error messages

### Phase 4: Advanced Features
1. Sum operators and ranges
2. Cross-buffer variable references
3. Export functionality
4. Integration with org-mode
5. Integration with tables 

## Key Design Decisions

1. **Use Emacs Calc**: Leverage built-in calc for mathematical operations rather than implementing arithmetic
2. **Buffer-local Variables**: Each buffer maintains its own variable namespace
3. **Overlay-based Display**: Results shown as overlays, not inserted into buffer text
4. **Minor Mode**: Implement as minor mode for easy integration with existing major modes
5. **Regex-based Parsing**: Simple regex patterns for initial implementation, can be enhanced later

## File Structure

```
numerals-mode/
├── numerals-mode.el          # Main mode definition and public API
├── numerals-parser.el        # Line parsing and syntax recognition
├── numerals-calc.el          # Calculator integration and evaluation
├── numerals-variables.el     # Variable storage and dependency management
├── numerals-display.el       # Overlay management and visual display
├── test/
│   ├── test-parser.el        # Unit tests for parser
│   ├── test-calc.el          # Unit tests for calculator
│   └── test-integration.el   # Integration tests
├── README.md                 # User documentation
└── examples/
    └── property-analysis.txt # Example use case file
```

## Example Use Case (Property Analysis)

The original motivation comes from property management calculations:

```
# Meredith Bay Corp. Property Analysis

## 41 Waukewan St
Monthly Rent = 2350.00
Taxes = 3560.22
Insurance = 2000.00
Yearly Expenses = Taxes + Insurance
Yearly Profit = (Monthly Rent * 12) - Yearly Expenses

## Portfolio Total
= Waukewan Profit + Other Properties...
```

## Dependencies

- Emacs 26.1+ (for built-in calc)
- No external packages required for core functionality
- Optional: dash.el for utility functions
- Optional: s.el for string manipulation

## Testing Strategy

1. **Unit Tests**: Test individual components in isolation
2. **Integration Tests**: Test mode functionality end-to-end
3. **Manual Testing**: Real-world use case scenarios
4. **Performance Testing**: Large files with many variables

## Future Enhancements

1. **Org-mode Integration**: Special handling for org tables and documents
2. **Export Functions**: Generate static HTML/PDF with calculated results
3. **Collaborative Features**: Shared variable namespaces
4. **Mathematical Plotting**: Integration with calc's plotting capabilities
5. **Package Distribution**: Submit to MELPA when stable

## Current Implementation Status (Updated Session)

### ✅ Completed Features

**Core Functionality:**
- Variable assignment and calculation system working
- Save-based updates (no real-time updating to avoid freezing)
- Overlay display system for results
- Integration with Emacs calc for mathematical operations

**Table Support (New):**
- Full support for both org-mode and markdown tables
- Excel-style cell references (A1, B2, C3, etc.)
- Range functions: SUM(A1:A3), AVERAGE(B2:B5), COUNT, MAX, MIN
- Formula syntax: `=B2*0.03`, `=SUM(C2:C5)` in table cells
- Clean result display replacing formulas with calculated values
- Org-mode integration that disables org's built-in table calculations

**File Structure:**
- `numerals-mode.el` - Main mode with table processing
- `numerals-parser.el` - Handles both text and table parsing
- `numerals-calc.el` - Calculator with timeout protection and error handling
- `numerals-variables.el` - Simplified variable storage (no dependency tracking)
- `numerals-display.el` - Overlay management
- `numerals-tables.el` - Complete table support module
- `numerals-dev.el` - Development helpers for reloading

### 🔧 Recent Major Changes

**Performance Fix:**
- Removed all real-time buffer updating (after-change-functions)
- Switched to save-based updates only (after-save-hook)
- Eliminated freezing issues caused by complex change tracking
- Removed dependency tracking system (simplified approach)

**Table Calculations:**
- Added comprehensive table detection for org-mode and markdown
- Implemented cell reference parsing (Excel-style: B2, C3, ranges: B2:B5)
- Created table-specific formula processing and overlay display
- Solved overlay alignment issues with exact-length padding
- Integrated with org-mode by disabling conflicting table features

**Display System:**
- Two overlay modes: regular calculations show "=> result", table cells replace formula text
- Table overlays use perfect alignment with padding to original formula length
- Right-aligned numbers in table cells for professional appearance

### ⚙️ Technical Implementation Details

**Table Processing Flow:**
1. Detect table type (org vs markdown) using line patterns
2. Parse table structure, skipping separator lines
3. Process each cell looking for `=formula` patterns
4. Search for exact formula text in buffer and replace with overlay
5. Maintain perfect table alignment through character-exact padding

**Org-mode Integration:**
- Disables org's built-in table calculations when numerals-mode is active
- Removes org table hooks and advises key functions
- Preserves org functionality when numerals-mode is disabled

**Development Tools:**
- `numerals-reload` function for quick development iteration
- `numerals-test-buffer` creates test environment
- Debug functions for table parsing analysis

### 🎯 Current Behavior

**Regular Text:**
```
x = 10              => 10
y = 20              => 20  
total = x + y       => 30
```

**Table Calculations:**
```
| Item    | Price | Qty | Total    |
|---------|-------|-----|----------|
| Apples  | 2.50  | 10  |     25.0 |  ← Was =B2*C2
| Oranges | 3.00  | 5   |     15.0 |  ← Was =B3*C3
| Total   |       |     |     40.0 |  ← Was =SUM(D2:D3)
```

### 📋 Known Limitations

**Table Display:**
- Columns with formulas appear wider than necessary due to padding requirements
- Perfect alignment vs. compact columns is a fundamental tradeoff with overlay replacement
- Longer results may get truncated if they exceed original formula length

**Functionality:**
- No real-time updates (save-based only)
- No cross-buffer variable references
- No dependency tracking between variables
- Limited to basic arithmetic and provided functions

### 🚀 Ready for Use

The mode is now fully functional for:
- Property analysis calculations
- Financial modeling with tables
- Scientific calculations with variables
- Any scenario requiring literate calculations with table support

Example files provided:
- `examples/property-analysis.txt` - Variable-based calculations
- `examples/table-calculations.txt` - Table calculation examples

## Getting Started for Development

1. Load `numerals-dev.el` for development helpers
2. Use `M-x numerals-reload` to reload changes
3. Use `M-x numerals-test-buffer` for quick testing
4. Test with both regular text and table formats

## Comparison to Existing Solutions

| Feature | numerals-mode | literate-calc-mode | org-table | SES-mode |
|---------|---------------|-------------------|-----------|----------|
| Variable names with spaces | ✓ | ✓ | ✗ | ✗ |
| Live updates | ✓ | ✓ | Manual | ✓ |
| Natural syntax | ✓ | ✓ | Complex | Excel-like |
| Maintenance | Active | Stale | Active | Active |
| Overlay display | ✓ | ✓ | ✗ | ✗ |

This project aims to provide a maintained, reliable alternative with syntax specifically designed for literate calculation workflows.
