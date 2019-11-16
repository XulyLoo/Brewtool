brewtool                package:unknown                R Documentation

FERRAMENTA PARA CÁLCULO E CORREÇÃO DA OG E DO IBU NA PRODUÇÃO CASEIRA DE CERVEJA

Description:

     A função irá calcular o volume de água (em litros) ou o tempo adicional de
     fervura necessário para se corrigir a gravidade específica (SG) obtida, de
     forma a se alcançar a gravidade original (OG) alvo da receita. De modo opcional,
     a função também calculará o índice de amargor (IBU) da bebida, utilizando
     informações a respeito do(s) lúpulo(s) utilizado(s) fornecidas pelo usuário.

Usage:

     brewtool(SG, OG, t.mosto, t.cal = 20, vol, tempo.f = 60, IBU = FALSE, input)

Arguments:

SG: número com o valor da gravidade específica pós-fervura obtida pelo usuário,
por medida com densímetro (1.020 < SG < 1.120). O valor é arredondado para 3 casas
decimais.

OG: número com o valor da gravidade original alvo (1.020 < OG < 1.120). O valor
é arredondado para 3 casas decimais.

t.mosto: número no intervalo 0 <= t.mosto =< 85 indicando a temperatura do mosto,
em graus Celsius, no momento da medida de SG.

t.cal: número maior que zero indicando a temperatura de calibração do densímetro
utilizado, em graus Celsius (default = 20).

vol: vetor numérico contendo dois valores maiores que zero, indicando os volumes do
mosto antes e após a fervura, respectivamente, em litros (vol = c(vol pre-fervura,
vol pos-fervura)).

tempo.f: numero maior que zero indicando o tempo total de fervura, em minutos
(default = 60).

IBU:  argumento lógico que, quando verdadeiro, permite o cálculo do índice de
amargor (IBU) utilizando o método de Tinseth (default = FALSE).

input: objeto da classe data.frame contendo as informações sobre peso (em gramas),
momento de adição (valor em minutos após o início da fervura) e porcentagem de alfa
ácidos de cada lúpulo utilizado. As colunas do data frame devem, obrigatoriamente,
ser nomeadas 'peso', 'tempo' e 'aa', e conter um lúpulo por linha. Objeto necessário
apenas se IBU = TRUE.

Details:

  Para o cálculo do índice de amargor, o argumento 'IBU' deve ser igual a TRUE e um
  objeto da classe data.frame deve ser fornecido no argumento 'input', contendo as
  informações sobre todos os lúpulos adicionados durante o tempo de fervura. As
  informações sobre peso e porcentagem de alfa-ácidos dos lúpulos podem ser
  encontradas em suas embalagens.

Value:

  SG.pre : Valor estimado de SG antes do inicio da fervura.

  SG.cal : Valor da OG do usuario, calculado a partir de SG considerando t.cal e t.mosto.

  vol.adicional : Volume de água a ser adicionado para o ajuste de SG.cal para a
  OG alvo, SE SG.cal > OG.

  tempo.adicional : Tempo de fervura adicional para o ajuste de SG.cal para a OG
  alvo, SE SG.cal < OG.

  IBU : Valor de IBU calculado para o tempo de fervura e volume pós-fervura dados
  pelo usuário.

  IBU.pos : Valor de IBU estimado considerando o ajuste de SG.cal para OG, se necessário.

Warning:

     O cálculo da SG depende da temperatura do mosto ('t.mosto') no exato momento
     da medição e também da temperatura de calibração ('t.cal') do densímetro
     utilizado.

     Para o cálculo do IBU, a função necessita de uma tabela da classe data frame
     contendo, obrigatoriamente, as colunas 'peso', 'tempo' e 'aa' para os lúpulos
     utilizados. O valor 'tempo' representa o momento de adição do lúpulo (em
     minutos), contado a partir do início da fervura, que seria o tempo 0 (ver
     examples).

Author(s):

     Juliana Santana Borsoi
     e-mail: juliana-borsoi@outlook.com

References:

     Blog Homini Lúpulo: https://www.hominilupulo.com.br/
     DINSLAKEN, Daniel. Como calcular IBU. Blog Concerveja. Disponível em:
     <https://concerveja.com.br/calcular-ibu/>. Acesso em: 21 Jun. 2019.
     Hydrometer Temperature Correction. Blog Straight to the Pint. Disponível
     em: <https://straighttothepint.com/hydrometer-temperature-correction/>. Acesso
     em: 20 Jun. 2019.

Examples:

     brewtool(SG = 1.046, OG = 1.1, t.mosto = 80, t.cal = 15, vol = c(30, 25), tempo.f = 70)
     brewtool(SG = 1.033, OG = 1.061, t.mosto = 80, t.cal = 20, vol = c(27, 25))
     brewtool(SG = 1.065, OG = 1.058, t.mosto = 50, t.cal = 20, vol = c(25, 21))

     a = c(0, 0, 0, 20, 10) # Tempo = 0 --> adição do lúpulo no início da fervura
     b = c(22, 14, 15, 6.8, 14.4)
     c = c(3.5, 8, 3, 5, 7)
     df = data.frame(a, b, c)
     colnames(df) = c("tempo", "peso", "aa")
     brewtool(1.030, 1.065, 80, 20, c(25, 21), 60, TRUE, df)
     brewtool(1.08, 1.058, 73, 15, c(23, 18), 55, TRUE, df)

     peso = c(46, 27)
     tempo = c(0, 45)
     aa = c(7.5, 5)
     lupulos = data.frame(peso, tempo, aa)
     brewtool(1.055, 1.0609, 40, 20, c(36, 30), 60, TRUE, lupulos)
