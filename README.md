# Examen Programación en R
Examen R
Analisis de Señales presión tuberias en trazado concentraducto

**Realizado por: Paula Alvarez - Marcelo Carmona**

Revisar Resumen Catedra 01
<a href="https://github.com/19762014mcs/Trabajo-Catedra-01/blob/main/README.md" target="_blank">Enlace Github Catedra</a>

**Contexto**

El transporte de solidos por tuberias a larga distancia, y en especial el transporte de concentrado como pulpa o conocido en el ambito minero como concentraducto, requiere un estricto control de los parametros de operación, manteniendo un control en todo momento de presiones a lo largo de la longitud de este concentraducto, debido a que dicho transporte es realizado manteniendo la presurización de la tubería de modo de evitar complicaciones operacionales y permita el desplzamiento de la pulpa hasta el sector de alimentación a espesadores de concentrado comunmente.

Para el control y continuidad operacional de este concentraducto es relevante mantener monitoreando las presiones en las estaciones de monitoreo y de válvulas en el trazado longittudinal. La data que disponemos y que sera utilizada para el analisis respectivo, contempla cuatro estaciones de monitoreo y dos estaciones de válvulas, cada estacion de monitoreo dispone de dos trasmisores de presión donde el valor de presión es enviada remotamente a sala de control, este valor que indica la presion en punto de la tuberia en forma continua, es visualizada en pantalla de salas de sala de operaciones en instalaciones de ubicadas en la ciudad de Antofagasta, en el centro de gestión operativa.

Un transmisor de presión convierte la medición de la presión de un fluido en una señal eléctrica que puede ser transmitida a sistemas de control o visualización. Funciona detectando los cambios de presión y transformándolos en variaciones de voltaje, corriente o resistencia, que luego son amplificadas y convertidas en una señal estándar. 

Para el correcto control operacional del concentraducto es vital tomar las medidas operacionales necesarias para no exceder presiones en tuberias acorde al diseño de ingenieria establecido en su contrucción, por ello se requiere que los instrumentos (transmisores de presion) entreguen confiabilidad en la operación indicando señales permanentes que refleje los cambios que pudieran producirse en la operación de transporte de concentrado (cambios de fase agua /pulpa, variación de velocidad de bombas de desplazamiento positivo, cambios en reologías de transporte). Es relevante, del punto de vista de integridad de activos que estos instrumentos entreguen señales con la faibilidad necesaria para sostener el control operativo de un transporte de concentrado por tuberias, es importante que en los momentos de mayor exigencias que es cuando se transporta pulpa tengamos la seguridad que las señales que estan llegando a pantalla en sala de control reflefen en todo momento cambios que pudieran producirse en el proceso de modo de conocer las tendencias en la presurización de estas tuberias y tomar los resguardos necesarios para mantener la continuida operativa del sistema.

Por ello, en este trabajo se plantea analizar dichas señales de instrumentos ubicados en la primera estación de monitoreo (SM1) del trazado por la importancia que posee en el trazado de transporte de concentrado de la pulpa desde la estación de bombeo, esta estación posee dos transmisores de presion (SM1_1 y SM1_2), esto con el fin de identificar la operatividad de estos instrumentos, si guardan relación su medición , en que frecuencias se identifica una mayor coherencia, si existe relación entre cambios o eventos que se pueden producir en la operación y los detecte, comparación de energia de señales.

Para esto, utilizaremos transformada de wavelet en particular función wtc.AB (biwavelet) del paquete en R para calcular la coherencia wavelet entre dos series temporales. Esta función proporciona información sobre la dependencia entre las series en diferentes escalas de tiempo y frecuencia.



