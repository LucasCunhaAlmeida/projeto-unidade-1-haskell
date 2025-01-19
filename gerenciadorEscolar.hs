-- (Parte de Lucas)
-- Criar Aluno
-- Remover Aluno
-- Editar Aluno
-- Listar Alunos

-- Inserir notas, fazer media e listar alunos aprovados e reprovados

-- (Parte de Gabriel)
-- Cadastrar Disciplina
-- Remover Disciplina
-- Editar Disciplina
-- Listar Disciplinas

-- (Parte de Lorena)
-- Cadastrar professor
-- Fazer media salarial dos professores
-- Listar professores
-- Remover professor

-- Projeto da unidade 1 de Linguagens de programação
-- Autores: Lucas Cunha, Lorena Pereira, Gabriel Silveira
import System.IO

-- Criando o tipo algebrico Aluno
data Aluno = Aluno {
    nomeAluno :: String,
    dataNasAluno :: String,
    serie :: String
} deriving (Show, Eq)

-- Criação do tipo algebrico Professor
data Professor = Professor {
    nomeProf :: String,
    dataNasProf :: String,
    salario :: Float
} deriving (Show, Eq)

-- Criação do tipo algebrico Disciplina
data Disciplina = Disciplina {
    nomeDisc :: String,
    cargaHoraria :: Int
} deriving (Show, Eq)



-- Função para escrever dados de alunos em um arquivo
escreverAlunos :: FilePath -> [Aluno] -> IO ()
escreverAlunos caminho alunos = do
    handle <- openFile caminho WriteMode -- A função openFile abre o arquivo especificado pelo caminho no modo de escrita (WriteMode). Ela retorna um handle, que é uma referência ao arquivo aberto. 
    mapM_ (hPutStrLn handle . show) alunos -- show: converte cada aluno em uma string, hPutStrLn handle escreve essa string no arquivo associado ao
    hClose handle -- Adiciona uma linha para cada aluno

--Função para escrever dados de professores em um arquivo
escreverProf :: FilePath -> [Professor] -> IO ()
escreverProf caminho professores = do
    handle <- openFile caminho WriteMode
    mapM_ (hPutStrLn handle . show) professores
    hClose handle

--Função para escrever dados de disciplinas em um arquivo
escreverDisc :: FilePath -> [Disciplina] -> IO ()
escreverDisc caminho disciplinas = do
    handle <- openFile caminho WriteMode
    mapM_ (hPutStrLn handle . show) disciplinas
    hClose handle
