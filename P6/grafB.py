import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scienceplots
import os

plt.style.use(['science', 'notebook', 'grid'])

DELTA_T = 0.0001
MASS_FACTORS = {
    1: 1.0,
    100: 100.0,
    10000: 1000.0
}
MAX_ITER = 100000
TOTAL_TIME = MAX_ITER * DELTA_T

FILE_MAP = {
    1: "trajB1_out.dat",
    100: "trajB100_out.dat",
    10000: "trajB10000_out.dat"
}

def load_data(filename, total_time):
    try:
        df = pd.read_csv(filename, sep='\s+', header=None, names=['t', 'x', 'y'], 
                         skiprows=0)
        return df[df['t'] <= total_time]
    except FileNotFoundError:
        print(f"Aviso: Arquivo '{filename}' não encontrado.")
        return None
    except Exception as e:
        print(f"Erro ao ler o arquivo {filename}: {e}")
        return None

def main():
    fig, axes = plt.subplots(1, 3, figsize=(18, 6))
    axes = axes.flatten()
    
    case_index = 0
    
    fig.suptitle(f'Trajetória da Terra Perturbada pela Massa de Júpiter ({TOTAL_TIME:.0f} Anos de Simulação)', 
                 fontsize=16, y=1.02)
    
    for id_val, factor in MASS_FACTORS.items():
        filename = FILE_MAP[id_val]
        df = load_data(filename, TOTAL_TIME)
        
        if df is not None:
            ax = axes[case_index]
            legend_label = f"$M_J = {factor:.0f} \\times M_{{J, real}}$"
            
            ax.plot(df['x'], df['y'], label='Órbita da Terra', linewidth=1.0, color='C0')
            
            # Adiciona o Sol na origem
            ax.plot(0, 0, 'o', color='gold', markersize=8, label='Sol (Origem)')
            
            ax.set_xlabel('Posição $x$ (UA)', fontsize=10)
            ax.set_ylabel('Posição $y$ (UA)', fontsize=10)
            ax.set_title(f'{legend_label}', fontsize=12)
            ax.set_aspect('equal', adjustable='box')
            ax.legend(loc='upper right', fontsize=8)
            ax.grid(True)
            
            case_index += 1
        
    # Informações da simulação no rodapé
    plt.figtext(0.5, 0.01, 
                f'Parâmetros: $\\Delta t = {DELTA_T}$ anos. Condições Iniciais: Órbitas Circulares.', 
                ha='center', fontsize=10)

    # Salva a figura como um único PDF
    output_filename = "grafB.pdf"
    plt.tight_layout(rect=[0, 0.03, 1, 0.95]) 
    plt.savefig(output_filename)
    plt.close()
    print(f"Gráfico comparativo salvo como {output_filename}")

if __name__ == "__main__":
    main()