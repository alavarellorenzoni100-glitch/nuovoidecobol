#!/usr/bin/env python3
import os
import subprocess

def display_menu():
    """Displays the main menu of the IDE."""
    print("\n--- COBOL IDE ---")
    print("1. Create/Edit COBOL file")
    print("2. Compile COBOL file")
    print("3. Run compiled program")
    print("4. Exit")
    return input("Choose an option: ")

def create_or_edit_file():
    """Prompts for a filename and opens it in vim."""
    filename = input("Enter the filename (e.g., hello.cob): ")
    if not filename:
        print("Filename cannot be empty.")
        return
    try:
        subprocess.run(['vim', filename])
    except FileNotFoundError:
        print("Error: 'vim' is not installed. Please install it to use this feature.")

def compile_file():
    """Prompts for a COBOL file and compiles it."""
    filename = input("Enter the COBOL file to compile (e.g., hello.cob): ")
    if not filename.endswith(".cob"):
        print("Error: Invalid file extension. Please provide a .cob file.")
        return
    if not os.path.exists(filename):
        print(f"Error: File '{filename}' not found.")
        return
    
    executable_name = filename[:-4]
    try:
        result = subprocess.run(['cobc', '-x', '-o', executable_name, filename], capture_output=True, text=True)
        if result.returncode == 0:
            print(f"Successfully compiled '{filename}' to '{executable_name}'.")
        else:
            print(f"Error compiling '{filename}':")
            print(result.stderr)
    except FileNotFoundError:
        print("Error: 'cobc' is not installed. Please install GnuCOBOL.")

def run_program():
    """Prompts for an executable and runs it."""
    executable_name = input("Enter the program to run (e.g., hello): ")
    if not os.path.exists(executable_name):
        print(f"Error: Program '{executable_name}' not found.")
        return
    
    try:
        subprocess.run([f'./{executable_name}'])
    except Exception as e:
        print(f"An error occurred: {e}")

def main():
    """Main function to run the IDE."""
    while True:
        choice = display_menu()
        if choice == '1':
            create_or_edit_file()
        elif choice == '2':
            compile_file()
        elif choice == '3':
            run_program()
        elif choice == '4':
            print("Exiting COBOL IDE. Goodbye!")
            break
        else:
            print("Invalid option, please try again.")

if __name__ == "__main__":
    main()
