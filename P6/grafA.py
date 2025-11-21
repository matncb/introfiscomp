import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scienceplots
plt.style.use(['science', 'notebook', 'grid'])

R_INICIAL = 1.0     # r (em UA)
V0_INICIAL = 3.0    # v0 (velocidade inicial)
DELTA_T = 0.0001     # delta_t (passo de tempo)
FILENAME_INPUT = "grafA.dat"
FILENAME_OUTPUT = "grafA.pdf"


def calculate_area(x, y):
    """Calcula a área total coberta por uma série de pontos em relação à origem."""
    area = 0.5 * np.sum(np.abs(x[:-1] * y[1:] - x[1:] * y[:-1]))
    return area

# --- CARREGAMENTO DOS DADOS ---
try:
    # Carrega os dados da trajetória. Assume que o Fortran gerou 3 colunas: t, x, y
    df = pd.read_csv(FILENAME_INPUT, sep='\s+', header=None, skiprows=2, names=['t', 'x', 'y'])
except FileNotFoundError:
    print(f"Erro: Arquivo '{FILENAME_INPUT}' não encontrado. Certifique-se de que o código Fortran foi executado.")
    exit()
except Exception as e:
    print(f"Erro ao ler o arquivo: {e}")
    exit()

# --- CÁLCULO DE PERÍODO E DIVISÃO DA TRAJETÓRIA ---

# O período (T) é o último valor de tempo registrado
PERIODO_T = df['t'].iloc[-1]
Q1_T = PERIODO_T / 4.0
Q2_T = PERIODO_T * 2.0 / 4.0
Q3_T = PERIODO_T * 3.0 / 4.0

# Encontra os índices dos tempos que delimitam os quartos do período
idx_q1 = df[df['t'] <= Q1_T].index[-1]
idx_q2 = df[df['t'] <= Q2_T].index[-1]
idx_q3 = df[df['t'] <= Q3_T].index[-1]
idx_q4 = df.index[-1]

# Divide o DataFrame em quatro trimestres
q1 = df.iloc[:idx_q1 + 1]
q2 = df.iloc[idx_q1:idx_q2 + 1]
q3 = df.iloc[idx_q2:idx_q3 + 1]
q4 = df.iloc[idx_q3:idx_q4 + 1]

# Lista de trimestres para iteração
quarters = [q1, q2, q3, q4]
quarter_names = ['1º Quarto (t=0 a T/4)', '2º Quarto (T/4 a T/2)', '3º Quarto (T/2 a 3T/4)', '4º Quarto (3T/4 a T)']

# --- GERAÇÃO DOS GRÁFICOS (4 GRÁFICOS EM SEQUÊNCIA) ---

fig, axes = plt.subplots(2, 2, figsize=(12, 12))
axes = axes.flatten()

# Determina o limite máximo para os eixos (para todos os gráficos terem a mesma escala)
max_abs = df[['x', 'y']].abs().max().max() * 1.1

# Plota cada quarto do período
for i, q_data in enumerate(quarters):
    ax = axes[i]
    
    # Desenha a trajetória completa em cinza claro
    ax.plot(df['x'], df['y'], color='lightgray', linestyle=':', label='Órbita Completa')
    
    # Desenha o quarto atual (trajetória)
    ax.plot(q_data['x'], q_data['y'], color=f'C{i}', linewidth=2, label=quarter_names[i])

    # Preenche a área varrida
    ax.fill(np.append([0], q_data['x']), np.append([0], q_data['y']), 
            color=f'C{i}', alpha=0.3, label=f'Área Varrida Q{i+1}')
    
    # Calcula e exibe a área varrida
    area = calculate_area(q_data['x'].values, q_data['y'].values)
    
    # Posição do Sol (origem)
    ax.plot(0, 0, 'o', color='gold', markersize=10, label='Sol (Origem)')
    
    # Formatação
    ax.set_title(f'Quarto {i+1} da Trajetória (Área: {area:.6f} UA²)', fontsize=14)
    ax.set_xlabel('x (UA)')
    ax.set_ylabel('y (UA)')
    ax.set_aspect('equal', adjustable='box')
    ax.legend(loc='upper right', fontsize=10)
    
    # Define os limites para o mesmo zoom em todos os gráficos
    ax.set_xlim(-max_abs, max_abs)
    ax.set_ylim(-max_abs, max_abs)

# Informações da simulação no rodapé
fig.suptitle('Lei das Áreas: Órbita Elíptica (Trajetória no Plano X-Y)', fontsize=16, y=0.95)
fig.text(0.5, 0.02, 
         f'Parâmetros da Simulação: $r={R_INICIAL}$ UA, $v_0={V0_INICIAL}$ UA/ano, $\Delta t={DELTA_T}$ anos. Período total simulado: $T={PERIODO_T:.4f}$ anos.', 
         ha='center', fontsize=12)

# Salva a figura como PDF
plt.tight_layout(rect=[0, 0.03, 1, 0.93]) # Ajusta para o texto no rodapé
plt.savefig(FILENAME_OUTPUT)
plt.close()
