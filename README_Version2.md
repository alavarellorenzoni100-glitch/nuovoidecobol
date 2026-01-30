# nuovoidecobol — Guida rapida

Questo repository contiene una semplice "IDE" a menu per creare, modificare, compilare ed eseguire programmi COBOL. Sono presenti implementazioni in C, Python e Node.js, uno script di avvio e un tutorial per iniziare.

## Link rapidi
- Implementazione C: `cobol_ide.c`
- Implementazione Python: `cobol_ide.py`
- Implementazione Node.js: `cobol_ide.js`
- Script di avvio: `start-ide-cobol.sh`
- Tutorial COBOL: `COBOL_TUTORIAL.md`

## Requisiti
- GnuCOBOL (`cobc`) installato e nel PATH
- Per la build C: `gcc`
- Per la versione Node.js: `node` (opzionale)
- Per la versione Python: `python3` (opzionale)
- Un editor di testo (`vim` o `nano`)

## Installazione / Build

1) Compilare la versione C (genera il binario `cobol_ide_c`):

```bash
gcc -o cobol_ide_c cobol_ide.c
```

2) (Opzionale) Usare la versione Python:

```bash
python3 cobol_ide.py
```

3) (Opzionale) Usare la versione Node.js:

```bash
node cobol_ide.js
```

## Avvio
Avvio tramite script:

```bash
./start-ide-cobol.sh
```

(lo script nello repo esegue il binario C se presente)

## Uso (comandi nel menu)
1. Create/Edit COBOL file — apre un editor per modificare il file (.cob)  
2. Compile COBOL file — compila con `cobc`  
3. Run compiled program — esegue l'eseguibile generato  
4. Exit — esce dall'IDE

## Esempio rapido — Hello World
1. Avvia l'IDE:

```bash
./start-ide-cobol.sh
```

2. Scegli "1" per creare/modificare e crea `hello.cob` con questo contenuto:

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.
```

3. Compila (opzione 2 oppure manuale):

```bash
cobc -x -o hello hello.cob
```

4. Esegui (opzione 3 oppure manuale):

```bash
./hello
```

## File principali nel repository
- `cobol_ide.c` — implementazione in C dell'IDE a menu  
- `cobol_ide.py` — implementazione alternativa in Python  
- `cobol_ide.js` — implementazione alternativa in Node.js  
- `start-ide-cobol.sh` — script di avvio che esegue il binario C  
- `COBOL_TUTORIAL.md` / `read.md` — tutorial e guida all'uso

## Note e suggerimenti
- Lo script `start-ide-cobol.sh` esegue il binario C (`./cobol_ide_c`) se presente; puoi modificarlo per provare automaticamente le versioni Python o Node.js.
- Alcune implementazioni aprono `vim` per modificare i file; la documentazione menziona anche `nano`. Per coerenza, sostituisci o documenta l'editor preferito.
- Aggiungi un file `CONTRIBUTING.md` se vuoi linee guida per chi contribuisce.

## Contribuire
1. Forka il repository  
2. Crea un branch `feature/<nome>`  
3. Fai commit e push  
4. Apri una Pull Request descrivendo le modifiche

## Licenza
Questo progetto è rilasciato sotto la Licenza Apache License 2.0 — vedi il file `LICENSE`.