# COBOL Tutorial: Getting Started

This tutorial will guide you through the basics of COBOL programming using the provided IDE.

## What is COBOL?

COBOL (COmmon Business-Oriented Language) is one of the oldest programming languages, designed primarily for business, administrative, and government applications. It is known for its English-like syntax, making it relatively readable.

## Your First COBOL Program: "Hello, World!"

Let's create the classic "Hello, World!" program.

### Step 1: Create a New COBOL File

1.  Start the IDE: `./start-ide-cobol.sh`
2.  Choose option `1` (Create/Edit COBOL file).
3.  When prompted for the filename, type `hello.cob` and press Enter.

The `nano` editor will open.

### Step 2: Write the COBOL Code

Enter the following code into the `nano` editor:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
```

Let's break down this simple program:

*   **`IDENTIFICATION DIVISION.`**: This section identifies the program. Every COBOL program starts with this.
*   **`PROGRAM-ID. HELLO.`**: Gives the program a name. In this case, "HELLO".
*   **`PROCEDURE DIVISION.`**: This is where the actual executable instructions of the program are placed.
*   **`DISPLAY "Hello, World!".`**: This statement is used to output text to the console. The text "Hello, World!" is enclosed in double quotes.
*   **`STOP RUN.`**: This statement terminates the execution of the COBOL program.

**Important COBOL Syntax Notes:**

*   **Fixed Format:** Traditional COBOL has a fixed-column format (though modern compilers like GnuCOBOL are more lenient). The code above adheres to a common standard:
    *   Columns 1-6: Sequence numbers (optional, often left blank or used for line numbers by editors).
    *   Column 7: Indicator area (for comments, continuation lines, etc. `*` for comments, `-` for continuation).
    *   Columns 8-11: Area A (for Division, Section, Paragraph headers, and certain COBOL statements).
    *   Columns 12-72: Area B (for most COBOL statements).
*   **Periods:** Most COBOL statements are terminated by a period (`.`).
*   **Keywords:** COBOL uses a large number of reserved keywords.

### Step 3: Save and Exit the Editor

1.  To save the file in `nano`, press `Ctrl+O`.
2.  Press `Enter` to confirm the filename.
3.  To exit `nano`, press `Ctrl+X`.

You will return to the IDE's main menu.

### Step 4: Compile the Program

1.  From the main menu, choose option `2` (Compile COBOL file).
2.  When prompted, enter `hello.cob` and press Enter.

If there are no errors, you should see a message indicating successful compilation. An executable file named `hello` will be created in the same directory.

### Step 5: Run the Program

1.  From the main menu, choose option `3` (Run compiled program).
2.  When prompted, enter `hello` (the name of the executable) and press Enter.

You should see the output:

```
Hello, World!
```

### Step 6: Exit the IDE

Choose option `4` to exit the IDE.

## Next Steps

Now that you've created and run your first COBOL program, you can experiment with more complex programs. Explore COBOL's other divisions like `DATA DIVISION` for declaring variables, and `WORKING-STORAGE SECTION` for defining program data.

Good luck with your COBOL journey!
