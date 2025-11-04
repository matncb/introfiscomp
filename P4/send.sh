# Configurações 

NUMERO_USP="15479472"

USUARIO="a${NUMERO_USP}"
SERVIDOR="basalto.ifsc.usp.br"

# Lista de TODOS os arquivos possíveis para o envio
LISTA_COMPLETA_DE_ARQUIVOS="exercicio.f90 grafA.pdf grafB.pdf grafC.pdf"

# Caminho completo da pasta de destino no servidor
PASTA_DE_DESTINO="/public/fiscomp2025-2-guilherme/proj4/proj4_${NUMERO_USP}"

##################################################################################

# --- Lógica de verificação de arquivos ---
ARQUIVOS_A_ENVIAR=""
ARQUIVOS_IGNORADOS=""

echo "Verificando arquivos no diretório atual..."

for arquivo in $LISTA_COMPLETA_DE_ARQUIVOS; do
  if [ -f "$arquivo" ]; then
    # Se o arquivo existe, adiciona à lista de envio
    ARQUIVOS_A_ENVIAR="${ARQUIVOS_A_ENVIAR} ${arquivo}"
  else
    # Se não existe, adiciona à lista de ignorados
    ARQUIVOS_IGNORADOS="${ARQUIVOS_IGNORADOS} ${arquivo}"
  fi
done
# --- Fim da lógica de verificação ---


# Verifica se algum arquivo foi encontrado para envio
if [ -z "$ARQUIVOS_A_ENVIAR" ]; then
  echo "-----------------------------------------------------------"
  echo "❌ Nenhum arquivo do projeto foi encontrado para ser enviado."
  echo "==========================================================="
  exit 1
fi


# Monta o endereço completo do destino
DESTINO_COMPLETO="${USUARIO}@${SERVIDOR}:${PASTA_DE_DESTINO}"

# Log
echo "==========================================================="
echo "Enviando arquivos para o servidor..."
echo "==========================================================="
echo "Usuário: ${USUARIO}"
echo "Servidor: ${SERVIDOR}"
echo "Pasta de Destino: ${PASTA_DE_DESTINO}"
echo ""
echo "Arquivos ENCONTRADOS para envio: ${ARQUIVOS_A_ENVIAR}"

# Mostra os arquivos ignorados apenas se houver algum
if [ -n "$ARQUIVOS_IGNORADOS" ]; then
  echo "Arquivos IGNORADOS (não encontrados): ${ARQUIVOS_IGNORADOS}"
fi

echo "-----------------------------------------------------------"


# scp para enviar apenas os arquivos que foram encontrados
scp ${ARQUIVOS_A_ENVIAR} ${DESTINO_COMPLETO}

# Verifica se o comando anterior foi executado
if [ $? -eq 0 ]; then
  echo "-----------------------------------------------------------"
  echo "✅ Arquivos enviados com sucesso!"
  echo "Lembre-se de proteger seu diretório com o comando:"
  echo "ssh ${USUARIO}@${SERVIDOR} 'chmod 700 ${PASTA_DE_DESTINO}'"
  echo "==========================================================="
else
  echo "-----------------------------------------------------------"
  echo "❌ Erro: Falha ao enviar os arquivos."
  echo "Verifique se o diretório de destino já existe no servidor."
  echo "==========================================================="
fi