# Compilador
FC := gfortran
# Flags de compilação
FFLAGS := -Wall -Wextra

# Diretórios
SRC_DIR := exemplos
BUILD_DIR := builds

# Encontra todos os arquivos Fortran no diretório de exemplos
SOURCES := $(wildcard $(SRC_DIR)/*.f90) $(wildcard $(SRC_DIR)/*.f) $(wildcard $(SRC_DIR)/*.for)
EXECUTABLES := $(patsubst $(SRC_DIR)/%, $(BUILD_DIR)/%, $(basename $(SOURCES)))

.PHONY: all clean

all: $(BUILD_DIR) $(EXECUTABLES)

# Cria o diretório de builds se não existir
$(BUILD_DIR):
	mkdir -p $@

# Regra genérica para compilar cada programa
$(BUILD_DIR)/%: $(SRC_DIR)/%.*
	$(FC) $(FFLAGS) $< -o $@

clean:
	rm -rf $(BUILD_DIR)