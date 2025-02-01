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
-- Fazer media salarial dos professores (horas * valor da hora)
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

-- Função para criar um professor
cadProfessor :: IO ()
cadProfessor = do --O bloco "do" é usado para encadear uma sequência de ações de IO.
    putStrLn "Digite o ID do professor:"
    idStr <- getLine
    putStrLn "Digite o nome do professor:"
    nome <- getLine
    putStrLn "Digite a data de nascimento do professor:"
    dataNasc <- getLine
    putStrLn "Digite as horas do professor:"
    horaStr <- getLine
    let hora = read horaStr :: Int -- criação de uma variável local hora que recebe o valor de horaStr convertido para inteiro
    let id = read idStr :: Int -- criação de uma variável local id que recebe o valor de idStr convertido para inteiro
    --let professor = Professor nome dataNasc salario -- criação de um professor com os dados informados
    escreverProf "professores.txt" (Professor id nome dataNasc hora) -- chamada da função escreverProf para salvar os dados do professor no arquivo professores.txt
    --writeFile "professores.txt" (show professor)
    putStrLn "Dados do professor salvos no arquivo professor.txt"

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
    "Professor(a) " ++ "Id " ++ show id ++ " " ++ show nome ++ " " ++ show dataNasc ++ " " ++ show hora


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

-- Função para cadastrar um novo aluno
cadAluno :: IO ()
cadAluno = do
    putStrLn "Digite o nome do aluno:"
    nome <- getLine
    putStrLn "Digite a data de nascimento do aluno:"
    dataNasc <- getLine
    putStrLn "Digite a matrícula do aluno:"
    matriculaStr <- getLine
    putStrLn "Digite a série do aluno:"
    serie <- getLine
    let matricula = read matriculaStr :: Int
    let aluno = Aluno 0 0 0 0 nome dataNasc matricula serie
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
    "Aluno(a) " ++ " Nome " ++ show nomeAluno ++ " Data nascimento " ++ show dataNasAluno ++ " Matrícula " ++ show matricula ++ " Série " ++ show serie

-- Função para inserir notas de um aluno
-- Sujeito a mudança ainda
inserirNotas :: IO ()
inserirNotas = do
    putStrLn "Digite a matrícula do aluno para inserir as notas:"
    matriculaStr <- getLine
    let procurarMatricula = read matriculaStr :: Int
    conteudo <- readFile "alunos.txt"
    let linhas = lines conteudo
    let alunos = mapMaybe leituraSegura linhas
    let alunoEncontrado = find (\aluno -> matricula aluno == procurarMatricula) alunos
    case alunoEncontrado of
        Nothing -> putStrLn "Nenhum aluno encontrado com essa matrícula."
        Just aluno -> do
            putStrLn "Digite a primeira nota:"
            nota1Str <- getLine
            putStrLn "Digite a segunda nota:"
            nota2Str <- getLine
            putStrLn "Digite a terceira nota:"
            nota3Str <- getLine
            putStrLn "Digite a quarta nota:"
            nota4Str <- getLine
            let notasAtualizadas = aluno {
                nota1 = read nota1Str :: Float,
                nota2 = read nota2Str :: Float,
                nota3 = read nota3Str :: Float,
                nota4 = read nota4Str :: Float
            }
            let alunosAtualizados = map (\a -> if matricula a == procurarMatricula then notasAtualizadas else a) alunos
            writeFile "alunos.txt" (unlines (map show alunosAtualizados))
            putStrLn "Notas atualizadas com sucesso!"

calcularMediaAluno :: IO ()
calcularMediaAluno = do
    putStrLn "Digite a matrícula do aluno para calcular a média:"
    matriculaStr <- getLine
    let procurarMatricula = read matriculaStr :: Int
    conteudo <- readFile "alunos.txt"
    let linhas = lines conteudo
    let alunos = mapMaybe leituraSegura linhas
    let alunoEncontrado = find (\aluno -> matricula aluno == procurarMatricula) alunos
    case alunoEncontrado of
        Nothing -> putStrLn "Nenhum aluno encontrado com essa matrícula."
        Just aluno -> do
            let media = (nota1 aluno + nota2 aluno + nota3 aluno + nota4 aluno) / 4
            let resultado = "Matrícula: " ++ show (matricula aluno) ++ 
                            ", Nome: " ++ nomeAluno aluno ++ 
                            ", Média: " ++ show media ++ "\n"
            -- Adiciona o resultado no arquivo "media.txt"
            appendFile "media.txt" resultado
            putStrLn $ "A média do aluno " ++ nomeAluno aluno ++ " foi salva no arquivo media.txt."

listarAprovadosReprovados :: IO ()
listarAprovadosReprovados = do
    -- Lê o conteúdo do arquivo com as médias
    conteudo <- readFile "media.txt"
    let linhas = lines conteudo
    -- Divide entre aprovados e reprovados com base na média
    let aprovados = filter (\linha -> extrairMedia linha >= 5.0) linhas
    let reprovados = filter (\linha -> extrairMedia linha < 5.0) linhas
    -- Exibe os resultados
    putStrLn "Alunos Aprovados:"
    mapM_ putStrLn aprovados
    putStrLn "\nAlunos Reprovados:"
    mapM_ putStrLn reprovados
  where
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