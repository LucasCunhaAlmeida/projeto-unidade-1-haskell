-- (Parte de Lucas)
-- Criar Aluno
-- Remover Aluno
-- Listar Alunos

-- Inserir notas, fazer media e listar alunos aprovados e reprovados

-- (Parte de Gabriel)
-- Cadastrar Disciplina
-- Remover Disciplina
-- Listar Disciplinas

-- (Parte de Lorena)
-- Cadastrar professor
-- Listar professores
-- Remover professor

-- Projeto da unidade 1 de Linguagens de programação
-- Autores: Lucas Cunha, Lorena Pereira, Gabriel Silveira
import System.IO
import Data.List (find)
import Data.Maybe (mapMaybe)

-- Criando o tipo algebrico Aluno
data Aluno = Aluno {
    nota1 :: Float,
    nota2 :: Float,
    nota3 :: Float,
    nota4 :: Float,
    nomeAluno :: String,
    dataNasAluno :: String,
    matricula :: Int,
    serie :: String
} deriving (Show, Read, Eq)

-- Criação do tipo algebrico Professor
data Professor = Professor {
    idProf :: Int,
    nomeProf :: String,
    dataNasProf :: String,
    hora :: Int
} deriving (Show, Eq, Read)

-- Criação do tipo algebrico Disciplina
data Disciplina = Disciplina {
    codigoDisc :: Int,
    nomeDisc :: String,
    cargaHoraria :: Int
} deriving (Show, Eq, Read)


-- Função auxiliar para verificar se o ID do professor já existe
idProfessorExiste :: Int -> [Professor] -> Bool
idProfessorExiste id [] = False
idProfessorExiste id (prof:profs)
    | idProf prof == id = True
    | otherwise = idProfessorExiste id profs

-- Função para criar um professor
cadProfessor :: IO ()
cadProfessor = do --O bloco "do" é usado para encadear uma sequência de ações de IO.
    putStrLn "Digite o ID do professor:"
    idStr <- getLine
    let id = read idStr :: Int

    -- Ler o conteúdo do arquivo de professores
    conteudo <- readFile "professores.txt"
    let linhas = lines conteudo
    let professores = mapMaybe leituraSegura linhas

    -- Verificar se o ID já existe
    if idProfessorExiste id professores
        then putStrLn "Já existe um professor com esse ID."
    else do
        putStrLn "Digite o nome do professor:"
        nome <- getLine
        putStrLn "Digite a data de nascimento do professor:"
        dataNasc <- getLine
        putStrLn "Digite as horas do professor:"
        horaStr <- getLine
        let hora = read horaStr :: Int
        let professor = Professor id nome dataNasc hora
        escreverProf "professores.txt" professor
        putStrLn "Dados do professor salvos no arquivo professores.txt"

-- Função para ler todos os professores
lerTodosProfessores :: IO ()
lerTodosProfessores = do
    conteudo <- readFile "professores.txt"
    let linhas = lines conteudo
    let professores = mapMaybe leituraSegura linhas
    putStrLn "Professores cadastrados:"
    mapM_ (putStrLn . formatarProfessor) professores

--Função para remover um professor
removerProfessor :: Int -> IO ()
removerProfessor idProcurado = do
    conteudo <- readFile "professores.txt"
    let linhas = lines conteudo
    --let professores = map read linhas :: [Professor]
    let professores = mapMaybe leituraSegura linhas
    --let professoresAtualizados = filter (\prof -> idProf prof /= idProcurado) professores
    let professoresAtualizados = filtrar (comparaId idProcurado) professores
    if tamanho professores == tamanho professoresAtualizados
        then putStrLn "Nenhum professor encontrado com esse ID para remover."
        else do
            writeFile "professores.txt" (unlines (map show professoresAtualizados))
            putStrLn "Professor removido com sucesso."

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f (x:xs) = [x | x <- xs, f x]

diferente :: Int -> Int -> Bool
diferente x y = x /= y  

-- Função para comparar o ID do professor
comparaId :: Int -> Professor -> Bool
comparaId idProcurado prof = diferente (idProf prof) idProcurado
    

-- Função auxiliar para leitura segura
--é uma função auxiliar que tenta ler uma string e convertê-la em um valor de um tipo que implementa a classe Read. Se a conversão for bem-sucedida, ela retorna Just com o valor lido. Caso contrário, retorna Nothing. safeRead é uma função polimórfica que pode operar em qualquer tipo a que implementa a classe Read. 
-- safeRead é uma função polimórfica que pode operar em qualquer tipo a que implementa a classe Read. Ela recebe uma String como entrada e retorna Maybe a, onde a é o tipo do valor que estamos tentando ler da string.
leituraSegura :: Read a => String -> Maybe a
leituraSegura s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing

--Função que retorna o tamanho de uma lista
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (_:xs) = 1 + tamanho xs

--Função para escrever dados de professores em um arquivo
escreverProf :: FilePath -> Professor -> IO ()
escreverProf caminho professor = do
    handle <- openFile caminho AppendMode
    hPutStrLn handle (show professor)
    hClose handle

formatarProfessor :: Professor -> String
formatarProfessor (Professor id nome dataNasc hora) =
    "Professor(a) " ++ "Id: " ++ show id ++ " Nome: " ++ show nome ++ " Data Nas: " ++ show dataNasc ++ " Horas " ++ show hora


-- Função para escrever uma lista de disciplinas
escreverDisc :: FilePath -> Disciplina -> IO ()
escreverDisc caminho disciplinas = do
    handle <- openFile caminho WriteMode
    hPutStrLn handle (show disciplinas)
    hClose handle
    putStrLn "Disciplina criada com sucesso!"


-- Função para criar novos alunos e adicionar ao arquivo
escreverAluno :: FilePath -> Aluno -> IO ()
escreverAluno caminho novoAluno = do
    handle <- openFile caminho AppendMode
    hPutStrLn handle (show novoAluno)
    hClose handle
    putStrLn "Aluno criado com sucesso!"
-- Função auxiliar para verificar se a matrícula do aluno já existe
matriculaAlunoExiste :: Int -> [Aluno] -> Bool
matriculaAlunoExiste matricula [] = False
matriculaAlunoExiste matriculaAluno (aluno:alunos)
    | matricula aluno == matriculaAluno = True
    | otherwise = matriculaAlunoExiste matriculaAluno alunos

-- Função para cadastrar um novo aluno
-- Função para cadastrar um novo aluno
cadAluno :: IO ()
cadAluno = do
    putStrLn "Digite a matrícula do aluno:"
    matriculaStr <- getLine
    let matricula = read matriculaStr :: Int
    
    -- Ler o conteúdo do arquivo de alunos
    conteudo <- readFile "alunos.txt"
    let linhas = lines conteudo
    let alunos = mapMaybe leituraSegura linhas
    
    -- Verificar se a matrícula já existe
    if matriculaAlunoExiste matricula alunos
        then putStrLn "Já existe um aluno com essa matrícula."
        else do
            putStrLn "Digite o nome do aluno:"
            nome <- getLine
            putStrLn "Digite a data de nascimento do aluno:"
            dataNasc <- getLine
            putStrLn "Digite a série do aluno:"
            serie <- getLine
            putStrLn "Digite a nota 1 do aluno:"
            nota1Str <- getLine
            let nota1 = read nota1Str :: Float
            putStrLn "Digite a nota 2 do aluno:"
            nota2Str <- getLine
            let nota2 = read nota2Str :: Float
            putStrLn "Digite a nota 3 do aluno:"
            nota3Str <- getLine
            let nota3 = read nota3Str :: Float
            putStrLn "Digite a nota 4 do aluno:"
            nota4Str <- getLine
            let nota4 = read nota4Str :: Float
            let aluno = Aluno nota1 nota2 nota3 nota4 nome dataNasc matricula serie
            escreverAluno "alunos.txt" aluno
            putStrLn "Dados do aluno salvos no arquivo alunos.txt"


--Função para remover um Aluno
removerAluno :: Int -> IO ()
removerAluno idProcurado = do
    conteudo <- readFile "alunos.txt"
    let linhas = lines conteudo
    let alunos = mapMaybe leituraSegura linhas
    let alunosAtualizados = filtrar (comparaMat idProcurado) alunos
    if tamanho alunos == tamanho alunosAtualizados
        then putStrLn "Nenhum aluno encontrado com essa matricula para remover."
        else do
            writeFile "alunos.txt" (unlines (map show alunosAtualizados))
            putStrLn "Aluno removido com sucesso."

-- Função para comparar o ID do aluno
comparaMat :: Int -> Aluno -> Bool
comparaMat matProcurado aluno = diferente (matricula aluno) matProcurado

lerTodosAlunos :: IO ()
lerTodosAlunos = do
    conteudo <- readFile "alunos.txt"
    let linhas = lines conteudo
    let alunos = mapMaybe leituraSegura linhas
    putStrLn "Alunos Matriculados:"
    mapM_ (putStrLn . formatarAluno) alunos

formatarAluno:: Aluno -> String
formatarAluno (Aluno nota1 nota2 nota3 nota4 nomeAluno dataNasAluno matricula serie) =
    "Aluno(a) " ++ " Nome: " ++ show nomeAluno ++ " Data nascimento: " ++ show dataNasAluno ++ " Matrícula: " ++ show matricula ++ " Série: " ++ show serie

-- Função auxiliar para comparar a matrícula do aluno
comparaMatricula :: Int -> Aluno -> Bool
comparaMatricula procurarMatricula aluno = matricula aluno == procurarMatricula

-- Função auxiliar para calcular a média de um aluno
calcularMediaAluno :: Aluno -> String
calcularMediaAluno aluno =
    let media = (nota1 aluno + nota2 aluno + nota3 aluno + nota4 aluno) / 4
    in "Matrícula: " ++ show (matricula aluno) ++ 
       ", Nome: " ++ nomeAluno aluno ++ 
       ", Média: " ++ show media
       
listarAprovadosReprovados :: IO ()
listarAprovadosReprovados = do
    -- Lê o conteúdo do arquivo com as médias
    conteudo <- readFile "media.txt"
    let linhas = lines conteudo
    -- Divide entre aprovados e reprovados com base na média
    let aprovados = filter alunoAprovado linhas
    let reprovados = filter alunoReprovado linhas
    -- Exibe os resultados
    putStrLn "Alunos Aprovados:"
    mapM_ putStrLn aprovados
    putStrLn "\nAlunos Reprovados:"
    mapM_ putStrLn reprovados

-- Função auxiliar para verificar se o aluno está aprovado
alunoAprovado :: String -> Bool
alunoAprovado linha = extrairMedia linha >= 5.0

-- Função auxiliar para verificar se o aluno está reprovado
alunoReprovado :: String -> Bool
alunoReprovado linha = extrairMedia linha < 5.0

-- Função auxiliar para extrair a média de uma linha
extrairMedia :: String -> Double
extrairMedia linha =
    let partes = words linha
        mediaStr = last partes -- A média é o último valor da linha
    in read mediaStr :: Double

-- Função para cadastrar uma nova disciplina
cadDisciplinas :: IO ()
cadDisciplinas = do
    putStrLn "Digite o codigo da disciplina:"
    idStr <- getLine
    putStrLn "Digite o nome da disciplina:"
    nome <- getLine
    putStrLn "Digite a carga horaria dessa disciplina:"
    horariaStr <- getLine
    let horaria = read horariaStr :: Int
    let id = read idStr :: Int
    escreverDisc "disciplinas.txt" (Disciplina id nome horaria)
    putStrLn "Dados das disciplas salvas no arquivo disciplinas.txt"


    -- Função para ler todas as disciplinas
lerTodasDisciplinas :: IO ()
lerTodasDisciplinas = do
    conteudo <- readFile "disciplinas.txt"
    let linhas = lines conteudo
    let disciplinas = mapMaybe leituraSegura linhas
    putStrLn "Disciplinas cadastradas:"
    mapM_ (putStrLn . formatarDisciplinas) disciplinas


-- Função para formatar a saida da String disciplina
formatarDisciplinas :: Disciplina -> String
formatarDisciplinas (Disciplina codigo nome carga) =
    "- " ++ show codigo ++  " Disciplina de" ++ " " ++ nome ++ " - Carga horaria: " ++ show carga ++ " horas."


-- Função para comparar o ID da disciplina
compararId :: Int -> Disciplina -> Bool
compararId idProcurado disc = diferente (codigoDisc disc) idProcurado


--Função para remover uma disciplina
removerDisciplinas :: Int -> IO ()
removerDisciplinas idProcurado = do
    conteudo <- readFile "disciplinas.txt"
    let linhas = lines conteudo
    let disciplinas = mapMaybe leituraSegura linhas
    let disciplinasAtt = filtrar (compararId idProcurado) disciplinas
    if tamanho disciplinas == tamanho disciplinasAtt
        then putStrLn "Nenhuma disciplina encontrada com esse ID para remover."
        else do
            writeFile "disciplinas.txt" (unlines (map show disciplinasAtt))
            putStrLn "Disciplina removida com sucesso!!"

-- Função para calcular a média de todos os alunos
calcularMediaTodosAlunos :: IO ()
calcularMediaTodosAlunos = do
    conteudo <- readFile "alunos.txt"
    let linhas = lines conteudo
    let alunos = mapMaybe leituraSegura linhas
    let resultados = map calcularMediaAluno alunos
    writeFile "media.txt" (unlines resultados)
    putStrLn "Médias de todos os alunos foram salvas no arquivo media.txt."

-- Função auxiliar para calcular a média de um aluno
calcularMediaAluno :: Aluno -> String
calcularMediaAluno aluno =
    let media = (nota1 aluno + nota2 aluno + nota3 aluno + nota4 aluno) / 4
    in "Matrícula: " ++ show (matricula aluno) ++ 
       ", Nome: " ++ nomeAluno aluno ++ 
       ", Média: " ++ show media