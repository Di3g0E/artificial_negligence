paises = read.table(file.choose(), header = T, sep = ";", stringsAsFactors = T)
head(paises)



#   -----------------------------------------------------------Ejercicio 4 --------------------------------------------------------

# Vemos cuál es el tipo de dato de 'paises$Esp.vida.Fem' y 'paises$Esp.vida.Masc' => Núm. Entero
typeof(paises$Esp.vida.Fem); typeof(paises$Esp.vida.Masc)

# Vemos cómo se distribuyen los datos => Los hombres tienen una esperanza de vida mucho menor (65.89 frente a 71.21)
summary(paises$Esp.vida.Fem); summary(paises$Esp.vida.Masc)

# Visualizamos los histogramas de los datos para poder realizar una comparación más visual
par(mfrow = c(1, 2))
hist(paises$Esp.vida.Masc, xlab = 'Años', ylab = 'Frecuencia', main = 'Hist. Hombres', col = 5); hist(paises$Esp.vida.Fem, xlab = 'Años', ylab = 'Frecuencia', main = 'Hist. Mujeres', col = 4)
# Como podemos ver, la mayor parte de las mujeres se encuentran entre los 65-85 años mientras que los hombres están entre los 60-75. Lo que supone una diferencia de casi 6 años como hemos visto representado en 'summary'.



#   --------------------------------------------------------- Ejercicio 5 ----------------------------------------------------------

# Comparamos la esperanza de vida de las mujeres con otras variables (Religión, Núm. hijos, Alfabetización, Calorías, Pib per cápita)
  
# Representación de los datos respecto a la religión:
table(paises$Religion)
# Las religiones son variables cualitativas, por lo que primero veo cuántas clases diferentes y cuántos datos tiene cada clase con la función table().

plot(paises$Religion, main = 'Frec. Religiones', col = 7, xlab = c("Hind", "Jud", "Animista", "Budista", "Catolica", "Musul", "Ortodoxa", "Protes"))
# Una vez sabemos los datos de los que disponemos creo un gráfico de barras para poder ver de forma más visual los datos de cada religión, aunque este paso no es estríctamente necesario.

plot(paises$Religion, paises$Esp.vida.Fem, col = 7)
# Para compararlo con la esperanza de vida femenina creo un boxplot en el que podemos ver mucha información acerca de la relación entre estas dos variables. Las religiones Hindú, Judía y Animista únicamente cuentan con un valor, por lo que podemos descartar estas religiones para realizar la comparación respecto al resto de religiones. Las religiones Budista y Ortodoxa también cuentan con pocos datos y como podemos ver su representación cambia bastante.

plot(paises$Religion, paises$Pais, col = 7)
# Este gráfico aporta más información acerca de los países en los que están presentes las diferentes religiones, en la que podemos ver en cuántos países se encuentra cada religión, como las religiones con más datos son la Protestante, Musulmana y Católica ahora vamos a comparar los países con la esperanza de vida de las mujeres debido a que es posible que haya un mayor número de mujeres creyentes en países desarroyados, lo que probocaría que suba la media de la esperanza de vida.

plot(paises$Esp.vida.Fem, paises$Religion, col = paises$Pais, main = "Esp.Vida.Fem. - Relig. - Países(color)")
# Nos fijamos en las columnas con mayor cantidad de datos para realizar una valoración más precisa. En el gráfico podemos ver que hay países que tienen una esperanza de vida mucho menor, como el que se representa con el color rojo, mientras que otros países tienen una esperanza de vida superior como los que tienen el color azul oscuro o gris.



# Representación de los datos respecto al núm. de hijos:
plot(paises$Hijpromedio, paises$Esp.vida.Fem, xlab = 'Núm. Hijos', ylab = 'Esp.Vida.Fem', main = 'N.Hijos - Esp.Vida.Fem', col = 5)
# En este caso comparamos con la cantidad de hijos con la espernza de vida, podemos ver cómo con una menor cantidad de hijos se tiene una mayor esperanza de vida claramente, pero esto puede deberse a la forma de vida o cultura de cada país, por lo que a continuación visualizamos otro punto de vista:

plot(paises$Hijpromedio, paises$Pais, xlab = 'Núm. Hijos', ylab = 'País', main = 'N.Hijos - País', col = 5)
# En este gráfico vemos que la mayor parte de los datos se encuentran a la izquierda de la gráfica, lo que nos indica que hay muchos países en los que se tienen tres hijos o menos en comparación con los que tienen cuatro o más.

par(mfrow = c(1,2)); plot(paises$Hijpromedio, paises$Pais, xlab = 'N.Hijos', ylab = 'País', main = 'N.Hijos - País', col = 5); plot(paises$Esp.vida.Fem, paises$Pais, xlab = 'Esp.vida.Fem', ylab = 'País', main = 'Esp.Vida.Fem - País', col = 5); par(mfrow = c(1, 1))
# En la enterior gráfica únicamente se podía ver la cantidad de hijos por cada país, pero ahora podemos comparar las gráficas en las que se comparan el número de hijos y la esperanza de vida femenina con cada país y podemos ver que se distribuyen igual pero la de la izquierda tiene la mayor parte de datos a la izquierda mientras que la gráfica de la derecha los tiene al otro lado, esto se debe a que en los países en los que las mujeres tienen menos hijos también tienen mayor esperanza de vida, confirmando lo interpretado en el cometario anterior.



# Representación de los datos respecto a la alfabetización:
plot(paises$Esp.vida.Fem, paises$Alfabet, xlab = 'Mujeres', ylab = 'Alfabet', main = 'Esp.Vida.Fem - Alfabet', col = 4)
# Ahora vamos a comparar características que podrían ser diferenciales de cada país con la esperanza de vida femenina. En este caso comparamos la esperanza de vida con la alfabetización y podemos observar que cuanta mayor esperanza de vida mayor es la alfabetización, esto podría deberse a que al vivir más tiempo también están más tiempo formándose, pero si lo comparamos con cada país podemos ver que una mayor alfabetización coincide con los países con mayor esperanza de vida.

par(mfrow = c(1,3)); plot(paises$Alfabet, paises$Esp.vida.Fem, ylab = 'Esp.Vida.Fem', xlab = 'Alfabet', main = 'Alfabet - Esp.Vida.Fem', col = 4); plot(paises$Alfabet, paises$Pais, ylab = 'País', xlab = 'Alfabet', main = 'Alfabet - País', col = 4); plot(paises$Esp.vida.Fem, paises$Pais, xlab = 'Esp.Vida.Fem', ylab = 'País', main = 'Esp.Vida.Fem - País', col = 4); par(mfrow = c(1, 1))
# Como vemos en las gráficas y hemos dicho anteriormente la alfabetización coincide con una mayor esperanza de vida gracias a los países más desarrollados en los que hay una mayor alfabetización. Si comparamos la segunda y tercera gráficas podemos ver que son muy similares.



# Representación de los datos respecto a las calorías:
plot(paises$Esp.vida.Fem, paises$Calorias, xlab = 'Mujeres', ylab = 'Calorias', main = 'Esp.Vida.Fem - Calorias', col = 3)
# Como en la gráfica de la alfabetización cuanta mayor esperanza de vida mayor acceso a calorías tienen las mujeres (en este caso), aunque las gráficas tienen una tendencia deferente, mientras que en la gráfica de la alfabetización vemos una tendencia que se puede dibujar con una recta en la grafica de las calorías la recta se puede dibujar de 0 a 3200 ya que a partir de esta cantidad de calorías no aumenta la esperanza de vida. Realizamos las mismas comparaciones que con la alfabetización para ver si tienen la misma relación con cada país.

par(mfrow = c(1,3)); plot(paises$Calorias, paises$Esp.vida.Fem, ylab = 'Esp.Vida.Fem', xlab = 'Calorías', main = 'Calorías - Esp.Vida.Fem', col = 3); plot(paises$Calorias, paises$Pais, ylab = 'País', xlab = 'Calorías', main = 'Calorías - País', col = 3); plot(paises$Esp.vida.Fem, paises$Pais, xlab = 'Esp.Vida.Fem', ylab = 'País', main = 'Esp.Vida.Fem - País', col = 3); par(mfrow = c(1, 1))
# En este caso las calorías de cada país no tienen ninguna relación con el resto de representaciones ya que la distribución de puntos (que no tiene orden debido a la gran dispersión que representa) no se parece en ningún caso a la distribución en el resto de gráficas.



# Representación de los datos respecto al PIB.CAP:
plot(paises$Esp.vida.Fem, paises$PIB.CAP, xlab = 'Mujeres', ylab = 'PIB.CAP', main = 'Esp.Vida.Fem - PIB.CAP', col = 2)
# Por último comparamos la esperanza de vida con el PIB per cápita, y vemos que la distribución de los datos claramente dividida, una primera más horizontal que llega al rededor de los 75 años de longevidad mientras que la segunda tendencia sube en vertical. Esto se puede deber a la diferencia de país como en los casos anteriores, por lo que realizamos la comparación:

plot(paises$PIB.CAP, paises$Pais, xlab = 'PIB.CAP', ylab = 'Pais', main = 'PIB.CAP - Pais', col = 2)
# La distibución de puntos en la gráfica es similar a la del número de hijos por cada país, esto se debe a que hay un mayor número de países con menor PIB per cápita, ahora creamos un gráfico que nos muestre la esperanza de vida femenina respecto a estas variables:

par(mfrow = c(1,3)); plot(paises$PIB.CAP, paises$Pais, xlab = 'PIB.CAP', ylab = 'Pais', main = 'PIB.CAP - Pais', col = 2); plot(y = paises$PIB.CAP, x = paises$Esp.vida.Fem, xlab = 'Esp.Vida.Fem', ylab = 'PIB.CAP', main = 'Esp.Vida.Fem - PIB.CAP - País (color)', col = paises$Pais); plot(paises$Esp.vida.Fem, paises$Pais, xlab = 'Esp.Vida.Fem', ylab = 'País', main = 'Esp.Vida.Fem - País', col = 2); par(mfrow = c(1, 1))
# Comparando las gráficas representadas vemos que la distribución de loa puntos entre la primera y última gráfica son opuestas lo que se puede deber a que el PIB.CAP no tiene mucha relación con la esperanza de vida. Nos ha llamado la atención el salto que hay entre 8000y 12000 que se vé muy bien representado en la segunda gráfica pero qye también se puede apreciar en la primera. Esto podría deberse a la diferencia de países, pero como vemos representado con colores no es así.