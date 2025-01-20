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
    nomeDisc :: String,
    cargaHoraria :: Int
} deriving (Show, Eq)

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

--Função para escrever dados de disciplinas em um arquivo
escreverDisc :: FilePath -> [Disciplina] -> IO ()
escreverDisc caminho disciplinas = do
    handle <- openFile caminho WriteMode
    mapM_ (hPutStrLn handle . show) disciplinas
    hClose handle


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
    