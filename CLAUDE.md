# Claude Context - Numerals Mode Development

## Project Overview
Numerals-mode is an Emacs minor mode for literate calculations similar to Obsidian's Numerals plugin. It provides automatic variable references, live updates, and table calculations with overlay display.

## Recent Work Summary (July 2025)

### Issues Resolved
1. **✅ Nested SUM formula evaluation** - Fixed issue where SUM formulas referencing other SUM formulas returned 0
2. **✅ Table structure preservation** - Prevented overlay extension beyond formula boundaries that broke table formatting
3. **✅ Calculation accuracy** - Fixed Total Comp formulas that incorrectly referenced B2 instead of their own row's salary
4. **✅ Right-alignment consistency** - Ensured all numeric results are properly right-aligned within their cells

### ✅ Cross-Table Cell References - RESOLVED

#### Solution Implemented (July 2025)
Successfully implemented a comprehensive cross-table reference system that replaces the problematic TOTALS function approach with a robust, extensible solution.

#### New Features
1. **Multiple Reference Formats**:
   - `TableName.CellRef` (e.g., `Budget.E24`)
   - `TableName[row,col]` (e.g., `Budget[24,5]`)
   - `TableName.TOTALS[col]` (e.g., `Budget.TOTALS[0]`)
   - `@TableName.CellRef` (org-mode style)

2. **Global Cell Registry**: Automatically tracks calculated values from named tables for cross-reference use

3. **Seamless Integration**: Works transparently with existing variable and calculation systems

#### Usage Examples
```
#+NAME: Budget
| Employee | Salary | FICA    |
|----------|--------|---------|
| Colin    |  50000 | 3825.00 |
| TOTALS   |        | 3825.00 |

Total FICA Cost = Budget.TOTALS[0]        # Uses TOTALS row, column 0
Specific Cell = Budget.C2                 # Direct cell reference  
Row/Col Format = Budget[2,3]              # Row 2, Column 3
Combined = Budget.TOTALS[0] + Budget.TOTALS[1] + Budget.TOTALS[2]
```

#### Technical Implementation
- **New file**: `numerals-table-refs.el` - Core cross-table reference engine
- **Modified**: `numerals-calc.el` - Integrated table reference substitution
- **Modified**: `numerals-mode.el` - Added automatic cell value registration
- **Removed**: Old TOTALS function with regex issues

#### Key Improvements Over Previous Approach
1. **No regex parsing issues** - Uses proper string replacement functions
2. **Extensible syntax** - Multiple reference formats supported
3. **Automatic registration** - Values registered during calculation, no manual tracking needed
4. **Org-mode compatible** - Supports standard org-mode table naming conventions
5. **No circular dependencies** - Clean module separation with forward declarations

### Key Files Modified
- `numerals-tables.el`: Enhanced SUM formula evaluation to handle nested function calls
- `numerals-mode.el`: Added table alignment infrastructure and improved overlay positioning

### Test Case
Primary test file: `/Users/roambot/Library/Mobile Documents/iCloud~md~obsidian/Documents/Meredith Bay Management/GASCO/MBM-Structure.org`

Contains employee compensation table with:
- Variable definitions: CEO_d, COO_d, GAS_d, SIB_d, Fees
- Complex nested formulas: `=SUM(G2:H2)+B2*CEO_d`
- Mix of calculated and literal values

### Known Limitations
1. **Minor alignment inconsistency**: Different formula lengths cause slight visual misalignment in same column
2. **Color inconsistency**: Some values display in different colors (white vs green) - pending investigation

### Architecture Notes
- Uses overlay system to display calculated results without modifying source text
- Table parsing supports both org-mode and markdown formats
- Formula evaluation handles cell references (A1, B2) and ranges (A1:B3)
- Supports Excel-style functions: SUM, AVERAGE, COUNT, MAX, MIN
- Variable system allows named values for reuse in formulas

### Next Steps for Future Development
1. **Perfect column alignment**: Implement cell boundary detection for consistent column-wide alignment
2. **Color consistency**: Investigate and fix overlay color inconsistencies
3. **Performance optimization**: Consider caching for large tables
4. **Additional functions**: VLOOKUP, IF statements, date functions
5. **Error handling**: Better user feedback for formula errors

### Commands to Remember
- Load mode: `(load "/Users/roambot/bin/lisp-projects/numerals-mode/numerals-mode.el")`
- Recalculate: `C-c C-c` or `M-x numerals-recalculate`
- Clear overlays: `C-c C-k` or `M-x numerals-clear`
- Toggle overlays: `C-c C-t`

### Git Status
Current commits include table alignment improvements and nested SUM formula fixes. All changes committed and ready for further development.