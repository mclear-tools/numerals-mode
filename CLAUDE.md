# Claude Context - Numerals Mode Development

## Project Overview
Numerals-mode is an Emacs minor mode for literate calculations similar to Obsidian's Numerals plugin. It provides automatic variable references, live updates, and table calculations with overlay display.

## Recent Work Summary (July 2025)

### Major Issues Resolved
1. **✅ Nested SUM formula evaluation** - Fixed issue where SUM formulas referencing other SUM formulas returned 0
2. **✅ Table structure preservation** - Prevented overlay extension beyond formula boundaries that broke table formatting
3. **✅ Calculation accuracy** - Fixed Total Comp formulas that incorrectly referenced B2 instead of their own row's salary
4. **✅ Right-alignment consistency** - Ensured all numeric results are properly right-aligned within their cells
5. **✅ Dependency order resolution** - Fixed fundamental timing issues where table formulas couldn't reference variables
6. **✅ Race condition elimination** - Resolved inconsistent calculation results on each save/recalculation

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

### ✅ Dependency Resolution System - RESOLVED (July 2025)

#### Problem Identified
MBM-Structure.org showed critical dependency timing issues:
- Variables could reference table cells: `TotalEmployeeCost = Budget.E8 + Budget.F8 + Budget.G8` ✅
- But table formulas couldn't reference variables: `=NET * CEO_d` → "Error: Undef" ❌
- Results were inconsistent - same formula would randomly work or fail on different saves

#### Root Cause Analysis
1. **Processing order mismatch**: Tables processed before variables were defined
2. **Race conditions**: Multiple passes over same data created conflicting overlays  
3. **Duplicate processing**: Same calculations repeated multiple times with inconsistent results

#### Solution Implemented
**4-Pass Dependency Resolution System with Intelligent Overlay Management**

**Pass 1: Simple Variables**
- Processes only literals and basic math with no dependencies
- Examples: `CEO_d = 0.6`, `Fees = 300000`
- Tracks processed positions to prevent reprocessing

**Pass 2: Tables (First Time)**
- Processes tables that can use simple variables from Pass 1
- **Process once only** - tracked to prevent duplicate calculations
- Examples: `=B2*0.0765`, `=SUM(C2:C3)` work reliably

**Pass 3: Complex Variables**  
- Processes variables that reference tables or other variables
- Examples: `TotalEmployeeCost = Budget.E8`, `NET = Fees - TotalEmployeeCost`
- Skips already-processed positions to avoid conflicts

**Pass 4: Selective Table Reprocessing**
- **Only reprocesses tables containing formulas that reference variables**
- **Clears old overlays first** to prevent display conflicts
- Examples: `=NET * CEO_d` now calculates reliably to correct values

#### Key Technical Improvements
1. **Position Tracking**: `numerals-processed-positions` prevents duplicate processing
2. **Selective Reprocessing**: Only updates tables that need variable references
3. **Clean Overlay Management**: `numerals-clear-table-overlays` prevents conflicts
4. **Dependency Detection**: Smart classification of simple vs complex expressions

#### Results Achieved
- ✅ **Elimination of race conditions**: Same calculation gives same result every time
- ✅ **Reliable Distribution column**: Formulas like `=NET * CEO_d` show calculated values instead of "Error: Undef"
- ✅ **Consistent behavior**: No more random failures on save/recalculation  
- ✅ **Complex dependency chains**: `Simple vars → Tables → Complex vars → Table formulas` all work reliably

### Key Files Modified (Final State)
- `numerals-mode.el`: Complete rewrite of `numerals-update-buffer` with 4-pass system and overlay management
- `numerals-table-refs.el`: Enhanced cell lookup to support both calculated and literal values
- `numerals-tables.el`: Enhanced SUM formula evaluation to handle nested function calls

### Test Case
Primary test file: `/Users/roambot/Library/Mobile Documents/iCloud~md~obsidian/Documents/Meredith Bay Management/GASCO/MBM-Structure.org`

Now works reliably with:
- Variable definitions: CEO_d, COO_d, GAS_d, SIB_d, Fees
- Table formulas referencing variables: `=NET * CEO_d`, `=NET * SIB_d`
- Complex dependency chains: variables → tables → variables → tables
- Consistent results on every save/recalculation

### Current Status
**All major functionality working reliably**. No known critical limitations.

### Architecture Notes (Current Implementation)
- **Overlay system**: Displays calculated results without modifying source text
- **4-pass dependency resolution**: Ensures reliable processing order
- **Position tracking**: Prevents duplicate processing and race conditions
- **Selective reprocessing**: Only updates what needs to change
- **Table parsing**: Supports both org-mode and markdown formats
- **Formula evaluation**: Handles cell references (A1, B2) and ranges (A1:B3)
- **Excel-style functions**: SUM, AVERAGE, COUNT, MAX, MIN
- **Cross-table references**: TableName.CellRef, TOTALS[col] syntax
- **Variable system**: Named values with complex dependency support

### Future Enhancement Opportunities
1. **Performance optimization**: Consider caching for very large tables (100+ rows)
2. **Additional functions**: VLOOKUP, IF statements, date functions
3. **Enhanced error reporting**: More detailed error messages for formula debugging
4. **Table editing integration**: Live recalculation during table editing
5. **Export functionality**: Generate standalone HTML/PDF reports

### Commands to Remember
- **Load mode**: `(load "/Users/roambot/bin/lisp-projects/numerals-mode/numerals-mode.el")`
- **Recalculate**: `C-c C-c` or `M-x numerals-recalculate`
- **Clear overlays**: `C-c C-k` or `M-x numerals-clear`
- **Toggle overlays**: `C-c C-t`
- **Reload for development**: `M-x numerals-reload`

### Recent Git History (July 2025)
1. `5e05b58` - Implement reliable 4-pass dependency resolution with overlay management
2. `708bcf2` - Implement 5-pass dependency resolution system (superseded)
3. `7a17579` - Fix table cell references to support literal values  
4. `9f83e39` - Implement two-pass evaluation system for dependency resolution
5. `cff7644` - Rename cross-table reference module and update gitignore

**Status**: All major functionality working reliably. Ready for production use.