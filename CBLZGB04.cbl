       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBLZGB04.
      ******************************************************************
      * Author: GUILHERME GRUNER BIRCKHOLZ
      * Date:   19/01/2024
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CLIENTES ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-E1.

       SELECT CARGOS ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-E2.

       SELECT DEPARTAMENTOS ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-E3.

       SELECT FUNCIONARIOS ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-E4.

       SELECT PROJETOS ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-E5.

       SELECT PROJETOSFUNCIONARIOS ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-E6.

       SELECT CONTAS ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-E7.

       SELECT CLIENTES-S ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-S1.

       SELECT CARGOS-S ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-S2.

       SELECT DEPARTAMENTOS-S ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-S3.

       SELECT FUNCIONARIOS-S ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-S4.

       SELECT PROJETOS-S ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-S5.

       SELECT PROJETOSFUNCIONARIOS-S ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-S6.

       SELECT CONTAS-S ASSIGN TO
           'C:\Users\CLIENTE\DOWNLOADS\CLIENTE.txt'
       FILE STATUS IS AS-STATUS-S7.


       DATA DIVISION.
       FILE SECTION.

       FD CLIENTES
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-CLIENTES                     PIC X(454).
       01 FILLER REDEFINES ARQ-CLIENTES.
          05 ARQ-L-ID                      PIC 9(02).
          05 ARQ-L-NOME                    PIC X(100).
          05 ARQ-L-TELEFONE                PIC X(50).
          05 ARQ-L-EMAIL                   PIC X(50).
          05 ARQ-L-ENDERECO                PIC X(100).
          05 ARQ-L-DESCRICAO               PIC X(100).
          05 ARQ-L-CPF                     PIC X(14).
          05 ARQ-L-CNPJ                    PIC X(18).
          05 ARQ-L-STATUS                  PIC X(20).

       FD CARGOS
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-CARGOS                       PIC X(62).
       01 FILLER REDEFINES ARQ-CARGOS.
          05 ARQ-L-ID-CARGO                PIC 9(02).
          05 ARQ-L-NOME-CARGO              PIC X(50).
          05 ARQ-L-SAL-BASE                PIC 9(10).

       FD DEPARTAMENTOS
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-DEPARTAMENTOS                PIC X(54).
       01 FILLER REDEFINES ARQ-DEPARTAMENTOS.
          05 ARQ-L-ID-DEP                  PIC 9(02).
          05 ARQ-L-NOME-DEP                PIC X(50).
          05 ARQ-L-ID-RESPONSAVEL          PIC 9(02).

       FD FUNCIONARIOS
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-FUNCIONARIOS                 PIC X(590).
       01 FILLER REDEFINES ARQ-FUNCIONARIOS.
          05 ARQ-L-ID-FUNC                 PIC 9(02).
          05 ARQ-L-ID-CARGO-FUNC           PIC 9(02).
          05 ARQ-L-ID-DEP-FUNC             PIC 9(02).
          05 ARQ-L-NOME-FUNC               PIC X(100).
          05 ARQ-L-TELEFONE-FUNC           PIC X(50).
          05 ARQ-L-EMAIL-FUNC              PIC X(50).
          05 ARQ-L-ENDERECO-FUNC           PIC X(100).
          05 ARQ-L-CPF-FUNC                PIC X(14).
          05 ARQ-L-TIPO-CONTRATO           PIC X(50).
          05 ARQ-L-MODO-TRAB               PIC X(100).
          05 ARQ-L-FORMACAO                PIC X(100).
          05 ARQ-L-STATUS-FUNC             PIC X(20).

       FD PROJETOS
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-PROJETOS                     PIC X(454).
       01 FILLER REDEFINES ARQ-PROJETOS.
          05 ARQ-L-ID-PROJETO              PIC 9(02).
          05 ARQ-L-ID-DEP-PROJETO          PIC 9(02).
          05 ARQ-L-ID-CLIENTE-PROJ         PIC 9(02).
          05 ARQ-L-NOME-PROJ               PIC X(100).
          05 ARQ-L-DESCRICAO-PROJ          PIC X(200).
          05 ARQ-L-STATUS-PROJ             PIC X(50).
          05 ARQ-L-VALOR-PROJ              PIC 9(10).
          05 ARQ-L-DATA-ENTREGA-PROJ       PIC X(10).

       FD PROJETOSFUNCIONARIOS
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-PROJETOSFUNCIONARIOS         PIC X(04).
       01 FILLER REDEFINES ARQ-PROJETOSFUNCIONARIOS.
          05 ARQ-L-ID-PROJETO-FUNC-PROJ    PIC 9(02).
          05 ARQ-L-ID-FUNC-FUNC-PROJ       PIC 9(02).

       FD CONTAS
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-CONTAS                       PIC X(174).
       01 FILLER REDEFINES ARQ-CONTAS.
          05 ARQ-L-ID-CONTA                PIC 9(02).
          05 ARQ-L-ID-FUNC-CONTA           PIC 9(02).
          05 ARQ-L-AGENCIA-CONTA           PIC X(50).
          05 ARQ-L-NUMERO-CONTA            PIC X(60).
          05 ARQ-L-TIPO-CONTA              PIC X(50).

       FD CLIENTES-S
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-CLIENTE-S                    PIC X(551).

       FD CARGOS-S
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-CARGO-S                      PIC X(93).

       FD DEPARTAMENTOS-S
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-DEPARTAMENTO-S               PIC X(91).

       FD FUNCIONARIOS-S
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-FUNCIONARIO-S                PIC X(734).

       FD PROJETOS-S
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-PROJETO-S                    PIC X(475).

       FD PROJETOSFUNCIONARIOS-S
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-PROJETOSFUNCIONARIOS-S       PIC X(30).


       FD CONTAS-S
          RECORDING MODE IS F
          BLOCK CONTAINS 0 RECORDS.

       01 ARQ-CONTA-S                      PIC X(212).


       WORKING-STORAGE SECTION.
       01 AS-STATUS-E1              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-E2              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-E3              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-E4              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-E5              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-E6              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-E7              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-S1              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-S2              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-S3              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-S4              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-S5              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-S6              PIC 9(02) VALUE ZEROS.
       01 AS-STATUS-S7              PIC 9(02) VALUE ZEROS.
       01 AS-FIM1                   PIC X(01) VALUE 'N'.
       01 AS-FIM2                   PIC X(01) VALUE 'N'.
       01 AS-FIM3                   PIC X(01) VALUE 'N'.
       01 AS-FIM4                   PIC X(01) VALUE 'N'.
       01 AS-FIM5                   PIC X(01) VALUE 'N'.
       01 AS-FIM6                   PIC X(01) VALUE 'N'.
       01 AS-FIM7                   PIC X(01) VALUE 'N'.

       01 CLIENTES-FIELDS.
           05 CLIENTES-ID.
               10 FILLER PIC X(3) VALUE 'ID:'.
               10 ARQ-S-ID PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CLIENTES-NOME.
               10 FILLER PIC X(7) VALUE 'Name:'.
               10 ARQ-S-NOME PIC X(100).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CLIENTES-TELEFONE.
               10 FILLER PIC X(9) VALUE 'Telefone:'.
               10 ARQ-S-TELEFONE PIC X(50).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CLIENTES-EMAIL.
               10 FILLER PIC X(6) VALUE 'Email:'.
               10 ARQ-S-EMAIL PIC X(50).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CLIENTES-ENDERECO.
               10 FILLER PIC X(9) VALUE 'Endereco:'.
               10 ARQ-S-ENDERECO PIC X(100).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CLIENTES-DESCRICAO.
               10 FILLER PIC X(10) VALUE 'Descricao:'.
               10 ARQ-S-DESCRICAO PIC X(100).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CLIENTES-CPF.
               10 FILLER PIC X(4) VALUE 'CPF:'.
               10 ARQ-S-CPF PIC X(14).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CLIENTES-CNPJ.
               10 FILLER PIC X(5) VALUE 'CNPJ:'.
               10 ARQ-S-CNPJ PIC X(18).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CLIENTES-STATUS.
               10 FILLER PIC X(7) VALUE 'Status:'.
               10 ARQ-S-STATUS PIC X(20).

       01 CARGOS-FIELDS.
           05 CARGOS-ID.
               10 FILLER PIC X(3) VALUE 'ID:'.
               10 ARQ-S-ID-CARGO PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CARGOS-NOME.
               10 FILLER PIC X(7) VALUE 'Name:'.
               10 ARQ-S-NOME-CARGO PIC X(50).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CARGOS-SAL-BASE.
               10 FILLER PIC X(9) VALUE 'Sal-Base:'.
               10 ARQ-S-SAL-BASE PIC 9(10).

       01 DEPARTAMENTOS-FIELDS.
           05 DEPARTAMENTOS-ID.
               10 FILLER PIC X(3) VALUE 'ID:'.
               10 ARQ-S-ID-DEP PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 DEPARTAMENTOS-NOME.
               10 FILLER PIC X(7) VALUE 'Name:'.
               10 ARQ-S-NOME-DEP PIC X(50).
           05 FILLER PIC X(3) VALUE SPACES.
           05 DEPARTAMENTOS-ID-RESPONSAVEL.
               10 FILLER PIC X(15) VALUE 'ID-Responsavel:'.
               10 ARQ-S-ID-RESPONSAVEL PIC 9(02).

       01 FUNCIONARIOS-FIELDS.
           05 FUNCIONARIOS-ID.
               10 FILLER PIC X(3) VALUE 'ID:'.
               10 ARQ-S-ID-FUNC PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-ID-CARGO.
               10 FILLER PIC X(9) VALUE 'ID-Cargo:'.
               10 ARQ-S-ID-CARGO-FUNC PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-ID-DEP.
               10 FILLER PIC X(10) VALUE 'ID-Dep:'.
               10 ARQ-S-ID-DEP-FUNC PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-NOME.
               10 FILLER PIC X(7) VALUE 'Name:'.
               10 ARQ-S-NOME-FUNC PIC X(100).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-TELEFONE.
               10 FILLER PIC X(9) VALUE 'Telefone:'.
               10 ARQ-S-TELEFONE-FUNC PIC X(50).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-EMAIL.
               10 FILLER PIC X(6) VALUE 'Email:'.
               10 ARQ-S-EMAIL-FUNC PIC X(50).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-ENDERECO.
               10 FILLER PIC X(9) VALUE 'Endereco:'.
               10 ARQ-S-ENDERECO-FUNC PIC X(100).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-CPF.
               10 FILLER PIC X(4) VALUE 'CPF:'.
               10 ARQ-S-CPF-FUNC PIC X(14).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-TIPO-CONTRATO.
               10 FILLER PIC X(14) VALUE 'Tipo-Contrato:'.
               10 ARQ-S-TIPO-CONTRATO PIC X(50).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-MODO-TRAB.
               10 FILLER PIC X(10) VALUE 'Modo-Trab:'.
               10 ARQ-S-MODO-TRAB PIC X(100).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-FORMACAO.
               10 FILLER PIC X(9) VALUE 'Formacao:'.
               10 ARQ-S-FORMACAO PIC X(100).
           05 FILLER PIC X(3) VALUE SPACES.
           05 FUNCIONARIOS-STATUS.
               10 FILLER PIC X(7) VALUE 'Status:'.
               10 ARQ-S-STATUS-FUNC PIC X(20).

       01 PROJETOS-FIELDS.
           05 PROJETOS-ID.
               10 FILLER PIC X(3) VALUE 'ID:'.
               10 ARQ-S-ID-PROJETO PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 PROJETOS-ID-DEP.
               10 FILLER PIC X(7) VALUE 'ID-Dep:'.
               10 ARQ-S-ID-DEP-PROJETO PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 PROJETOS-ID-CLIENTE.
               10 FILLER PIC X(11) VALUE 'ID-Cliente:'.
               10 ARQ-S-ID-CLIENTE-PROJ PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 PROJETOS-NOME.
               10 FILLER PIC X(7) VALUE 'Name:'.
               10 ARQ-S-NOME-PROJ PIC X(100).
           05 FILLER PIC X(3) VALUE SPACES.
           05 PROJETOS-DESCRICAO.
               10 FILLER PIC X(10) VALUE 'Descricao:'.
               10 ARQ-S-DESCRICAO-PROJ PIC X(200).
           05 FILLER PIC X(3) VALUE SPACES.
           05 PROJETOS-STATUS.
               10 FILLER PIC X(7) VALUE 'Status:'.
               10 ARQ-S-STATUS-PROJ PIC X(50).
           05 FILLER PIC X(3) VALUE SPACES.
           05 PROJETOS-VALOR.
               10 FILLER PIC X(6) VALUE 'Valor:'.
               10 ARQ-S-VALOR-PROJ PIC 9(10).
           05 FILLER PIC X(3) VALUE SPACES.
           05 PROJETOS-DATA-ENTREGA.
               10 FILLER PIC X(13) VALUE 'Data-Entrega:'.
               10 ARQ-S-DATA-ENTREGA-PROJ PIC X(10).

       01 PROJETOSFUNCIONARIOS-FIELDS.
           05 PROJETOSFUNCIONARIOS-ID-PROJETO.
               10 FILLER PIC X(11) VALUE 'ID-Projeto:'.
               10 ARQ-S-ID-PROJETO-FUNC-PROJ PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 PROJETOSFUNCIONARIOS-ID-FUNC.
               10 FILLER PIC X(8) VALUE 'ID-Func:'.
               10 ARQ-S-ID-FUNC-FUNC-PROJ PIC 9(02).

       01 CONTAS-FIELDS.
           05 CONTAS-ID.
               10 FILLER PIC X(3) VALUE 'ID:'.
               10 ARQ-S-ID-CONTA PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CONTAS-ID-FUNC.
               10 FILLER PIC X(8) VALUE 'ID-Func:'.
               10 ARQ-S-ID-FUNC-CONTA PIC 9(02).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CONTAS-AGENCIA.
               10 FILLER PIC X(8) VALUE 'Agencia:'.
               10 ARQ-S-AGENCIA-CONTA PIC X(50).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CONTAS-NUMERO.
               10 FILLER PIC X(7) VALUE 'Numero:'.
               10 ARQ-S-NUMERO-CONTA PIC X(60).
           05 FILLER PIC X(3) VALUE SPACES.
           05 CONTAS-TIPO.
               10 FILLER PIC X(5) VALUE 'Tipo:'.
               10 ARQ-S-TIPO-CONTA PIC X(50).

       PROCEDURE DIVISION.
           PERFORM 1000-INICIALIZAR.
           PERFORM 2000-PROCESSAR .
           PERFORM 2100-PROCESSAR-CARGOS UNTIL AS-FIM1 EQUAL 'S'.
           PERFORM 2200-PROCESSAR-CARGOS UNTIL AS-FIM2 EQUAL 'S'.
           PERFORM 2300-PROCESSAR-CARGOS UNTIL AS-FIM3 EQUAL 'S'.
           PERFORM 2400-PROCESSAR-CARGOS UNTIL AS-FIM4 EQUAL 'S'.
           PERFORM 2500-PROCESSAR-CARGOS UNTIL AS-FIM5 EQUAL 'S'.
           PERFORM 2600-PROCESSAR-CARGOS UNTIL AS-FIM6 EQUAL 'S'.
           PERFORM 2700-PROCESSAR-CARGOS UNTIL AS-FIM7 EQUAL 'S'.
           PERFORM 3000-FINALIZAR.

       1000-INICIALIZAR        SECTION.
           READ CLIENTES.
           IF AS-STATUS-E1 NOT EQUALS ZEROS
               DISPLAY 'ARQUIVO VAZIO'
               MOVE 'S' TO AS-FIM1
           END-IF.

           READ CARGOS.
           IF AS-STATUS-E2 NOT EQUALS ZEROS
               DISPLAY 'ARQUIVO VAZIO'
               MOVE 'S' TO AS-FIM2
           END-IF.

           READ DEPARTAMENTOS.
           IF AS-STATUS-E3 NOT EQUALS ZEROS
               DISPLAY 'ARQUIVO VAZIO'
               MOVE 'S' TO AS-FIM3
           END-IF.

           READ FUNCIONARIOS.
           IF AS-STATUS-E4 NOT EQUALS ZEROS
               DISPLAY 'ARQUIVO VAZIO'
               MOVE 'S' TO AS-FIM4
           END-IF.

           READ PROJETOS.
           IF AS-STATUS-E5 NOT EQUALS ZEROS
               DISPLAY 'ARQUIVO VAZIO'
               MOVE 'S' TO AS-FIM5
           END-IF.

           READ PROJETOSFUNCIONARIOS.
           IF AS-STATUS-E6 NOT EQUALS ZEROS
               DISPLAY 'ARQUIVO VAZIO'
               MOVE 'S' TO AS-FIM6
           END-IF.

           READ CONTAS.
           IF AS-STATUS-E7 NOT EQUALS ZEROS
               DISPLAY 'ARQUIVO VAZIO'
               MOVE 'S' TO AS-FIM7
           END-IF.

       1000-INICILIZAR-FIM.
           EXIT.

       2000-PROCESSAR          SECTION.
       2000-PROCESSAR-FIM.
           EXIT.

       2100-PROCESSAR-CLIENTES SECTION.
           OPEN INPUT CLIENTES.
           IF AS-STATUS-E1 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-E1
           END-IF.



           MOVE ARQ-L-ID TO ARQ-S-ID
           MOVE ARQ-L-NOME TO ARQ-S-NOME
           MOVE ARQ-L-STATUS TO ARQ-S-STATUS
           MOVE ARQ-L-TELEFONE TO ARQ-S-TELEFONE
           MOVE ARQ-L-EMAIL TO ARQ-S-EMAIL
           MOVE ARQ-L-ENDERECO TO ARQ-S-ENDERECO
           MOVE ARQ-L-DESCRICAO TO ARQ-S-DESCRICAO
           MOVE ARQ-L-CPF TO ARQ-S-CPF
           MOVE ARQ-L-CNPJ TO ARQ-S-CNPJ

           OPEN OUTPUT CLIENTES-S.
           IF AS-STATUS-S1 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-S1
           END-IF.
           WRITE CLIENTES-FIELDS.

       2100-PROCESSAR-CLIENTES-FIM.
           EXIT.

       2200-PROCESSAR-CARGOS SECTION.
           OPEN INPUT CARGOS.
           IF AS-STATUS-S2 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-E2
           END-IF.



           MOVE ARQ-L-ID TO ARQ-S-ID
           MOVE ARQ-L-NOME TO ARQ-S-NOME
           MOVE ARQ-L-STATUS TO ARQ-S-STATUS
           MOVE ARQ-L-TELEFONE TO ARQ-S-TELEFONE
           MOVE ARQ-L-EMAIL TO ARQ-S-EMAIL
           MOVE ARQ-L-ENDERECO TO ARQ-S-ENDERECO
           MOVE ARQ-L-DESCRICAO TO ARQ-S-DESCRICAO
           MOVE ARQ-L-CPF TO ARQ-S-CPF
           MOVE ARQ-L-CNPJ TO ARQ-S-CNPJ

           OPEN OUTPUT CARGOS-S.
           IF AS-STATUS-S2 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-S2
           END-IF.
           WRITE CLIENTES-FIELDS.

       2200-PROCESSAR-CARGOS-FIM.
           EXIT.

       2300-PROCESSAR-DEPARTAMENTO SECTION.
           OPEN INPUT DEPARTAMENTOS.
           IF AS-STATUS-S3 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-E3
           END-IF.



           MOVE ARQ-L-ID TO ARQ-S-ID
           MOVE ARQ-L-NOME TO ARQ-S-NOME
           MOVE ARQ-L-STATUS TO ARQ-S-STATUS
           MOVE ARQ-L-TELEFONE TO ARQ-S-TELEFONE
           MOVE ARQ-L-EMAIL TO ARQ-S-EMAIL
           MOVE ARQ-L-ENDERECO TO ARQ-S-ENDERECO
           MOVE ARQ-L-DESCRICAO TO ARQ-S-DESCRICAO
           MOVE ARQ-L-CPF TO ARQ-S-CPF
           MOVE ARQ-L-CNPJ TO ARQ-S-CNPJ

           OPEN OUTPUT DEPARTAMENTOS-S.
           IF AS-STATUS-S3 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-S3
           END-IF.
           WRITE CLIENTES-FIELDS.

       2300-PROCESSAR-DEPARTAMENTO-FIM.
           EXIT.

       2400-PROCESSAR-FUNCIONARIOS SECTION.
           OPEN INPUT FUNCIONARIOS-S.
           IF AS-STATUS-S4 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-E4
           END-IF.



           MOVE ARQ-L-ID TO ARQ-S-ID
           MOVE ARQ-L-NOME TO ARQ-S-NOME
           MOVE ARQ-L-STATUS TO ARQ-S-STATUS
           MOVE ARQ-L-TELEFONE TO ARQ-S-TELEFONE
           MOVE ARQ-L-EMAIL TO ARQ-S-EMAIL
           MOVE ARQ-L-ENDERECO TO ARQ-S-ENDERECO
           MOVE ARQ-L-DESCRICAO TO ARQ-S-DESCRICAO
           MOVE ARQ-L-CPF TO ARQ-S-CPF
           MOVE ARQ-L-CNPJ TO ARQ-S-CNPJ

           OPEN OUTPUT FUNCIONARIOS-S.
           IF AS-STATUS-S4 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-E4
           END-IF.
           WRITE CLIENTES-FIELDS.

       2400-PROCESSAR-FUNCIONARIOS-FIM.
           EXIT.

       2500-PROCESSAR-PROJETOS SECTION.
           OPEN INPUT PROJETOS.
           IF AS-STATUS-E5 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-E5
           END-IF.



           MOVE ARQ-L-ID TO ARQ-S-ID
           MOVE ARQ-L-NOME TO ARQ-S-NOME
           MOVE ARQ-L-STATUS TO ARQ-S-STATUS
           MOVE ARQ-L-TELEFONE TO ARQ-S-TELEFONE
           MOVE ARQ-L-EMAIL TO ARQ-S-EMAIL
           MOVE ARQ-L-ENDERECO TO ARQ-S-ENDERECO
           MOVE ARQ-L-DESCRICAO TO ARQ-S-DESCRICAO
           MOVE ARQ-L-CPF TO ARQ-S-CPF
           MOVE ARQ-L-CNPJ TO ARQ-S-CNPJ

           OPEN INPUT PROJETOS-S.
           IF AS-STATUS-S5 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-S5
           END-IF.
           WRITE CLIENTES-FIELDS.

       2500-PROCESSAR-PROJETOS-FIM.
           EXIT.

       2600-PROCESSAR-PROJFUNC SECTION.
           OPEN INPUT PROJETOSFUNCIONARIOS.
           IF AS-STATUS-E6 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-E6
           END-IF.



           MOVE ARQ-L-ID TO ARQ-S-ID
           MOVE ARQ-L-NOME TO ARQ-S-NOME
           MOVE ARQ-L-STATUS TO ARQ-S-STATUS
           MOVE ARQ-L-TELEFONE TO ARQ-S-TELEFONE
           MOVE ARQ-L-EMAIL TO ARQ-S-EMAIL
           MOVE ARQ-L-ENDERECO TO ARQ-S-ENDERECO
           MOVE ARQ-L-DESCRICAO TO ARQ-S-DESCRICAO
           MOVE ARQ-L-CPF TO ARQ-S-CPF
           MOVE ARQ-L-CNPJ TO ARQ-S-CNPJ

           OPEN OUTPUT PROJETOSFUNCIONARIOS-S.
           IF AS-STATUS-S6 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-S6
           END-IF.
           WRITE CLIENTES-FIELDS.

       2600-PROCESSAR-PROJFUNC-FIM.
           EXIT.

       2700-PROCESSAR-CONTAS SECTION.
           OPEN INPUT CONTAS.
           IF AS-STATUS-E7 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-E7
           END-IF.



           MOVE ARQ-L-ID TO ARQ-S-ID
           MOVE ARQ-L-NOME TO ARQ-S-NOME
           MOVE ARQ-L-STATUS TO ARQ-S-STATUS
           MOVE ARQ-L-TELEFONE TO ARQ-S-TELEFONE
           MOVE ARQ-L-EMAIL TO ARQ-S-EMAIL
           MOVE ARQ-L-ENDERECO TO ARQ-S-ENDERECO
           MOVE ARQ-L-DESCRICAO TO ARQ-S-DESCRICAO
           MOVE ARQ-L-CPF TO ARQ-S-CPF
           MOVE ARQ-L-CNPJ TO ARQ-S-CNPJ

           OPEN INPUT CONTAS-S.
           IF AS-STATUS-S7 NOT EQUALS ZEROS
               DISPLAY 'ERROS NO OPEN' AS-STATUS-S7
           END-IF.
           WRITE CLIENTES-FIELDS.

       2700-PROCESSAR-CONTAS-FIM.
           EXIT.



       2000-PROCESSAR-FIM.
           EXIT.

       3000-FINALIZAR          SECTION.

       3000-FINALIZAR-FIM.
           EXIT.

       END PROGRAM CBLZGB04.
