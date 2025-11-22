import numpy as np
import matplotlib.pyplot as plt

def plot_trajectory(filename):

    try:
        data = np.loadtxt(filename)
        
        # As colunas são [tempo, x, y]
        t = data[:, 0]
        x = data[:, 1]
        y = data[:, 2]
        
        # Determina os parâmetros da simulação para exibir no título
        delta_t = t[1] - t[0] if len(t) > 1 else 0 
        num_pontos = len(t)
        
        # Cria a figura e o eixo
        fig, ax = plt.subplots(figsize=(8, 8))
        
        # Plota a trajetória
        ax.plot(x, y, label='Trajetória Planeta', linewidth=0.8)
        
        # Plota a posição do Sol (origem)
        ax.plot(0, 0, 'o', color='gold', markersize=8, label='Sol (Origem)')
        
        # Configurações do Gráfico
        ax.set_aspect('equal', adjustable='box')
        ax.set_xlabel('Posição x (UA)')
        ax.set_ylabel('Posição y (UA)')
        
        title = f"Trajetória da Órbita: $\Delta t$={delta_t:.4E} ({num_pontos} pontos)"
        ax.set_title(title)
        
        ax.grid(True, linestyle='--', alpha=0.6)
        ax.legend()
        
        plt.savefig("traj_test.pdf")

    except FileNotFoundError:
        print(f"Erro: Arquivo '{filename}' não encontrado.")
    except Exception as e:
        print(f"Ocorreu um erro ao processar o arquivo: {e}")

# Nome do arquivo de saída do Fortran
fortran_output_file = 'trajA1_out.dat' 
plot_trajectory(fortran_output_file)