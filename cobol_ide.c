#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void display_menu();
void create_or_edit_file();
void compile_file();
void run_program();

int main() {
    int choice;
    do {
        display_menu();
        printf("Choose an option: ");
        scanf("%d", &choice);
        getchar(); // consume the newline character

        switch (choice) {
            case 1:
                create_or_edit_file();
                break;
            case 2:
                compile_file();
                break;
            case 3:
                run_program();
                break;
            case 4:
                printf("Exiting COBOL IDE. Goodbye!\n");
                break;
            default:
                printf("Invalid option, please try again.\n");
        }
    } while (choice != 4);

    return 0;
}

void display_menu() {
    printf("\n--- COBOL IDE (C) ---\n");
    printf("1. Create/Edit COBOL file (save in editor)\n");
    printf("2. Compile COBOL file\n");
    printf("3. Run compiled program\n");
    printf("4. Exit IDE\n");
}

void create_or_edit_file() {
    char filename[100];
    printf("Enter the filename (e.g., hello.cob): ");
    fgets(filename, sizeof(filename), stdin);
    filename[strcspn(filename, "\n")] = 0; // remove newline character

    if (strlen(filename) == 0) {
        printf("Filename cannot be empty.\n");
        return;
    }

    char command[200];
    printf("To save your changes, press Ctrl+O, then ENTER.\n");
    printf("To exit the editor, press Ctrl+X.\n");
    snprintf(command, sizeof(command), "nano %s", filename);
    system(command);
}

void compile_file() {
    char filename[100];
    printf("Enter the COBOL file to compile (e.g., hello.cob): ");
    fgets(filename, sizeof(filename), stdin);
    filename[strcspn(filename, "\n")] = 0;

    if (strstr(filename, ".cob") == NULL) {
        printf("Error: Invalid file extension. Please provide a .cob file.\n");
        return;
    }

    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        printf("Error: File '%s' not found.\n", filename);
        return;
    }
    fclose(file);

    char executable_name[100];
    strncpy(executable_name, filename, strlen(filename) - 4);
    executable_name[strlen(filename) - 4] = '\0';

    char command[300];
    snprintf(command, sizeof(command), "cobc -x -o %s %s", executable_name, filename);
    int result = system(command);

    if (result == 0) {
        printf("Successfully compiled '%s' to '%s'.\n", filename, executable_name);
    } else {
        printf("Error compiling '%s'.\n", filename);
    }
}

void run_program() {
    char executable_name[100];
    printf("Enter the program to run (e.g., hello): ");
    fgets(executable_name, sizeof(executable_name), stdin);
    executable_name[strcspn(executable_name, "\n")] = 0;

    FILE *file = fopen(executable_name, "r");
    if (file == NULL) {
        printf("Error: Program '%s' not found.\n", executable_name);
        return;
    }
    fclose(file);

    char command[200];
    snprintf(command, sizeof(command), "./%s", executable_name);
    system(command);
}
