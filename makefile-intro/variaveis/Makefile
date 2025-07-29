# Todas as variáveis funcionam como texto, um grande copia e cola

VAR1 := hello
VAR2 := world

#$(VAR) lê uma variável

# Substituição de palavras
VAR3  := ${VAR2:world=planet}

.PHONY: all target1 target2 target3 # Não são arquivos físicos

# Por padrão, o comando make irá executar o primeiro alvo -- all

# all tem duas dependências, que seram executadas
all: target1 target2 target3
	@echo $(VAR3)

target1: target2
	@echo $(VAR2)

target2:
	@echo $(VAR1)

# @ e < guardam o alvo e a primeira dependência
target3: target1 target2
	@echo 'Alvo' $@
	@echo 'Primeira dependencia' $<	
	

# make target1 executa só target1
# make target2 executa só target2

# make VAR1=goodbye sobrescreve o valor da variável

# Fluxo
# Executa all --> dependências --> target 1 (não executa pq precisa do 2) --> target 2 (executa) ---> target1 (executa) ---> target3 (executa pq já foi 1 e 2) 