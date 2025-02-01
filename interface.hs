import GerenciadorEscolar

main :: IO ()
main = do
    putStrLn "Bem-vindo ao Gerenciador Escolar"
    menu

menu :: IO ()
menu = do
    putStrLn "\nEscolha uma opção:"
    putStrLn "1. Cadastrar Aluno"
    putStrLn "2. Ler Todos os Alunos"
    putStrLn "3. Remover Aluno"
    putStrLn "4. Calcular media de alunos"
    putStrLn "5. Listar alunos aprovados e reprovados"
    putStrLn "6. Cadastrar professor"
    putStrLn "7. Ler todos os professores"
    putStrLn "8. Remover professor"
    putStrLn "9. Cadastrar disciplina"
    putStrLn "10. Ler todas as disciplinas"
    putStrLn "11. Remover disciplina"
    putStrLn "12. Modificar Nota de Aluno"
    putStrLn "13. Modificar professor"
    putStrLn "14. Modificar disciplina"
    putStrLn "15. Sair"
    putStr "Opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            cadAluno
            menu
        "2" -> do
            lerTodosAlunos
            menu
        "3" -> do
            putStr "Digite a matricula do aluno a ser removido: "
            idAluno <- getLine
            removerAluno (read idAluno)
            menu
        "4" -> do
            calcularMediaTodosAlunos
            menu
        "5" -> do
            listarAprovadosReprovados
            menu
        "6" -> do
            cadProfessor
            menu
        "7" -> do
            lerTodosProfessores
            menu
        "8" -> do
            putStr "Digite o ID do professor a ser removido: "
            idProfessor <- getLine
            removerProfessor (read idProfessor)
            menu
        "9" -> do
            cadDisciplinas
            menu
        "10" -> do
            lerTodasDisciplinas
            menu
        "11" -> do
            putStr "Digite o ID da disciplina a ser removida: "
            idDisciplina <- getLine
            removerDisciplinas (read idDisciplina)
            menu
        "12" -> do
            putStr "Digite a matricula do aluno: "
            idAluno <- getLine
            removerAluno (read idAluno)
            putStr "\n***Digite os dados do aluno novamente***\n\n"
            cadAluno
            menu
        "13" -> do
            putStr "Digite o ID do professor a ser modificado: "
            idProfessor <- getLine
            removerProfessor (read idProfessor)
            putStr "\n***Digite os novos dados do professor***\n\n"
            cadProfessor
            menu
        "14" -> do
            putStr "Digite o ID da disciplina a ser modificada: "
            idDisciplina <- getLine
            removerDisciplinas (read idDisciplina)
            putStr "\n***Digite os novos dados da disciplina***\n\n"
            cadDisciplinas
            menu
        "15" -> do
            putStrLn "Saindo..."
        _   -> do
            putStrLn "Opção inválida, tente novamente."
            menu