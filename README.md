# C-family-Compiler
A compiler for a simple language similar to c language.
Written in Lex, Yacc, C.

<b>Compiller has 3 steps:</b>
1. Lexical analysis reuslt will be a Abstract syntax tree for Syntax analysis.
2. Syntax analysis.
3. Three address code (3AC) generation.
<hr/>

<b>Demo:</b>
<br/><br/>
![ALT "demo video"](https://github.com/Yahavba/C-family-Compiler/blob/master/Videos/Demo.gif)
<hr/>
<b>In order to run the application:</b>

1. clone this repository.
2. open Linux command.
3. download bison lex and yacc: sudo apt-get install bison.
3. go to project location.
4. change codeTest.txt by language rules or leave the template code.
5. run command: yacc -d parser.y 
6. run command: lex scanner.l
7. run command: cc -o test y.tab.c -ll
8. run command: ./test < projectPath/codeTest.txt

Please let me know if you find bugs or something that needs to be fixed.

<b>Hope you enjoy.</b>
