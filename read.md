# COBOL IDE

This is a simple COBOL IDE that allows you to create, edit, compile, and run COBOL programs.

## How to Use

1.  **Start the IDE:**
    Run the `start-ide-cobol.sh` script:
    ```bash
    ./start-ide-cobol.sh
    ```

2.  **Navigate the Menu:**
    The IDE will present you with a menu:
    ```
    --- COBOL IDE (C) ---
    1. Create/Edit COBOL file (save in editor)
    2. Compile COBOL file
    3. Run compiled program
    4. Exit IDE
    ```

3.  **Create/Edit COBOL file (Option 1):**
    *   Enter the filename (e.g., `hello.cob`).
    *   The `nano` editor will open.
    *   To save your changes, press `Ctrl+O`, then `ENTER`.
    *   To exit the editor, press `Ctrl+X`.

4.  **Compile COBOL file (Option 2):**
    *   Enter the COBOL filename (e.g., `hello.cob`).
    *   The IDE will use `cobc` to compile the file. An executable will be created with the same name as the COBOL file (e.g., `hello`).

5.  **Run compiled program (Option 3):**
    *   Enter the name of the compiled program (e.g., `hello`).
    *   The IDE will execute the program.

6.  **Exit IDE (Option 4):**
    *   Exits the IDE.

## Requirements

*   `cobc` (GnuCOBOL compiler)
*   `nano` (text editor)
*   `gcc` (C compiler, for building the IDE itself)

## Source Code

*   `cobol_ide.c`: The C source code for the IDE.
*   `start-ide-cobol.sh`: A shell script to start the IDE.
*   `cobol_ide.js`: (Optional) A Node.js version of the IDE.
*   `cobol_ide.py`: (Optional) A Python version of the IDE.
