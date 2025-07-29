# Vamos ilustrar que uma depência só será rodada se for necessário

.PHONY: default

default: dependencia.txt dependencia2.txt
	@cat dependencia.txt
	@cat dependencia2.txt

dependencia.txt:
	echo "Conteúdo da dependência" > $@

dependencia2.txt: dependencia.txt
	cp $< $@

# make (primeira execução) --> cria o arquivo e mostra o conteudo
# make (segunda execução) ---> só mostra o conteudo

# Se modificarmos o arquivo manualmente ---> só mostra o conteudo de dependencia.txt e sobrescreve dependencia2.txt

# Make só recria arquivos se suas dependências estiverem mais recentes ou se o alvo não existir. Sem dependências declaradas, ele assume que o arquivo está sempre atualizado após a criação inicial.