const readline = require('readline');
const { spawn } = require('child_process');
const fs = require('fs');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

function displayMenu() {
  console.log("\n--- COBOL IDE (Node.js) ---");
  console.log("1. Create/Edit COBOL file");
  console.log("2. Compile COBOL file");
  console.log("3. Run compiled program");
  console.log("4. Exit");
  rl.question("Choose an option: ", (choice) => {
    handleMenuChoice(choice);
  });
}

function handleMenuChoice(choice) {
  switch (choice) {
    case '1':
      createOrEditFile();
      break;
    case '2':
      compileFile();
      break;
    case '3':
      runProgram();
      break;
    case '4':
      console.log("Exiting COBOL IDE. Goodbye!");
      rl.close();
      break;
    default:
      console.log("Invalid option, please try again.");
      displayMenu();
      break;
  }
}

function createOrEditFile() {
  rl.question("Enter the filename (e.g., hello.cob): ", (filename) => {
    if (!filename) {
      console.log("Filename cannot be empty.");
      displayMenu();
      return;
    }
    const editor = spawn('vim', [filename], { stdio: 'inherit' });
    editor.on('exit', () => {
      displayMenu();
    });
  });
}

function compileFile() {
  rl.question("Enter the COBOL file to compile (e.g., hello.cob): ", (filename) => {
    if (!filename.endsWith(".cob")) {
      console.log("Error: Invalid file extension. Please provide a .cob file.");
      displayMenu();
      return;
    }
    if (!fs.existsSync(filename)) {
      console.log(`Error: File '${filename}' not found.`);
      displayMenu();
      return;
    }

    const executableName = filename.slice(0, -4);
    const compiler = spawn('cobc', ['-x', '-o', executableName, filename]);

    compiler.stdout.on('data', (data) => {
      console.log(`stdout: ${data}`);
    });

    compiler.stderr.on('data', (data) => {
      console.error(`stderr: ${data}`);
    });

    compiler.on('close', (code) => {
      if (code === 0) {
        console.log(`Successfully compiled '${filename}' to '${executableName}'.`);
      } else {
        console.log(`Error compiling '${filename}'.`);
      }
      displayMenu();
    });
  });
}

function runProgram() {
  rl.question("Enter the program to run (e.g., hello): ", (executableName) => {
    if (!fs.existsSync(executableName)) {
      console.log(`Error: Program '${executableName}' not found.`);
      displayMenu();
      return;
    }

    const program = spawn(`./${executableName}`);

    program.stdout.on('data', (data) => {
      console.log(`stdout: ${data}`);
    });

    program.stderr.on('data', (data) => {
      console.error(`stderr: ${data}`);
    });

    program.on('close', (code) => {
      displayMenu();
    });
  });
}

function main() {
  displayMenu();
}

main();
