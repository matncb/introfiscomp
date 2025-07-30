# Compilador
FC := gfortran
# Flags de compilação
FFLAGS := -Wall -Wextra -O3

# Diretórios
SRC_DIR := exemples
BUILD_DIR := builds

SOURCES := $(wildcard $(SRC_DIR)/*.f90) # wildcard busca aquivos correspondentes a um padrão (.f90)

# Alternativa --> basename remove extensão do arquivo,
# $(patsubst origem/%, destino/%, lista)
EXECUTABLES := $(patsubst $(SRC_DIR)/%.f90, $(BUILD_DIR)/%, $(SOURCES)) 

.PHONY: all clean # Diz que all e clean são alvos ficitícios, para não procurar por nenhum arquivo all ou clean

all: $(BUILD_DIR) $(EXECUTABLES) # all depende de build dir e executables

clean:
	rm -rf $(BUILD_DIR)

# $@ le a variável com mesmo nome do alvo
# $< le primeira dependência

# Cria o diretório de builds se não existir
$(BUILD_DIR):
	mkdir -p $@ 

# Regra genérica para compilar cada programa
# % representa qualquer sequência de caracteres --> não inclui extensões de arquivos
# %.* representa qualquer arquivo de qualquer extensão
$(BUILD_DIR)/%: $(SRC_DIR)/%.*
	$(FC) $(FFLAGS) $< -o $@
