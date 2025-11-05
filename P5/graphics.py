import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import scienceplots

plt.style.use(['science', 'notebook', 'grid'])


r=3
x_data = np.arange(0,1,0.01)
y_data = r*x_data*(1-x_data)

plt.plot(x_data, y_data)
plt.plot(x_data, x_data)
plt.show()