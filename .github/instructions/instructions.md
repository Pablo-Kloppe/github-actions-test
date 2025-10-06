---
applyTo: '**/*.cbl'
---

# ✅ Quick Checklist for COBOL (Fixed Format)

- Do **not** reindent, auto-format, or trim spaces.  
- Respect **fixed-format layout**:  
  - Divisions/sections/paragraphs start in **Area A** (column 8).  
  - Statements start in **Area B** (column 12).  
- Keep `IDENTIFICATION DIVISION.` aligned exactly as in the source.  
- Do not touch columns 1–7 or beyond 72.  
- Only change what the issue explicitly requests.  

---

# 📑 COBOL Editing Guidelines (Detailed)

## COBOL Fixed Format Rules
1. **Columns 1–6**: sequence numbers (leave unchanged).  
2. **Column 7**: indicators: `*` (comment), `/` (page eject), `D` (debugging).  
3. **Columns 8–11 (Area A)**: divisions, section headers, paragraph names.  
   - Example: `       IDENTIFICATION DIVISION.`  
4. **Columns 12–72 (Area B)**: statements and clauses.  
   - Example: `           DISPLAY "HELLO WORLD"`.  
5. **Columns 73–80**: ignored by compiler (leave untouched).  

## General Editing Rules
- Preserve **exact spacing** before every keyword.  
- Do not change case of reserved words.  
- Do not wrap lines beyond column 72.  
- Do not remove comment lines (`*` in column 7).  
- Insert new code aligned with Area A or Area B, depending on construct.  

## Acceptance Criteria
- The diff shows **only the requested code changes**.  
- `IDENTIFICATION DIVISION.` stays aligned in Area A.  
- No whitespace or indentation changes outside the intended edit.  