
#Trabajo práctico manejo de EPH con lenguaje R

####Paquetes utilizados####

#install.packages("eph")
#install.packages("tidyverse")
#install.packages("kableExtra")
#install.packages("treemap")

library(eph)
library(tidyverse)
library(kableExtra)
library(treemap)

####Datos####

ind_2_20 <- get_microdata(year = 2020, trimester = 2, type = "individual")

#####Transformación de los datos####

ind_2_20 <- ind_2_20 %>% 
  filter(P21>0) %>% 
  filter(PP07G1 %in% c(1,2),  
         PP07G2 %in% c(1,2), 
         PP07G3 %in% c(1,2),
         PP07G4 %in% c(1,2),
         PP07H  %in% c(1,2)) %>% 
  mutate(Ocupado_inf = ifelse(PP07G1 == 1 & PP07G2 == 1 & PP07G3 == 1 & PP07G4 == 1 & PP07H == 1, 0, 1)) %>%
  filter(ESTADO %in% c(1,3))  %>%
  filter(PP04A %in% c(1,2,3)) %>% 
  filter(CH06 >= 18 & CH06 <= 65) %>% 
  select(ESTADO, CAT_OCUP, PP04A, DECOCUR, PP04C, NIVEL_ED, CH04, PONDERA, Ocupado_inf, CH06, REGION, P21, PONDIIO) %>% 
  rename(Tipo_Organizacion = PP04A, Numero_Empleados = PP04C, Genero = CH04, Edad = CH06)

ind_2_20 <- ind_2_20 %>% mutate(Tipo_Ocupacion = case_when(Ocupado_inf == 1 ~ "Informal",
                                                           Ocupado_inf == 0 ~ "Formal"),
                                Categoria_empresa = case_when(Numero_Empleados <= 6                           ~ "Micro",
                                                              Numero_Empleados  > 6 & Numero_Empleados <= 8   ~ "Pequeña",
                                                              Numero_Empleados >= 9 & Numero_Empleados <=10   ~ "Mediana T1",
                                                              Numero_Empleados == 11                          ~ "Mediana T2",
                                                              Numero_Empleados == 12                          ~ "Grande",
                                                              Numero_Empleados == 99                          ~ "Sin dato"))

ind_2_20 <- ind_2_20 %>% mutate(Tipo_Organizacion = case_when(Tipo_Organizacion == 1 ~ "Publica",
                                                              Tipo_Organizacion == 2 ~ "Privada",
                                                              Tipo_Organizacion == 3 ~ "Otra"),
                                Genero = case_when(Genero == 1 ~ "Hombre",
                                                   Genero == 2 ~ "Mujer"),
                                NIVEL_ED = case_when(NIVEL_ED == 1 ~ "Prim. incompleta",
                                                     NIVEL_ED == 2 ~ "Prim. completa",
                                                     NIVEL_ED == 3 ~ "Sec. incompleta",
                                                     NIVEL_ED == 4 ~ "Sec. completa",
                                                     NIVEL_ED == 5 ~ "Univ. incompleta",
                                                     NIVEL_ED == 6 ~ "Univ. completa",
                                                     NIVEL_ED == 7 ~ "Sin instrucción",
                                                     NIVEL_ED == 9 ~ "Ns./Nr.") )

colnames(ind_2_20)
dim(ind_2_20)

ind_2_20 <- ind_2_20 %>% mutate(Categoria_empresa = as_factor(Categoria_empresa),
                                Genero = as_factor(Genero),
                                NIVEL_ED = as_factor(NIVEL_ED),
                                Ocupado_inf = as_factor(Ocupado_inf),
                                Tipo_Organizacion = as_factor(Tipo_Organizacion),
                                DECOCUR = as.numeric(DECOCUR))

levels(ind_2_20$Categoria_empresa)

ind_2_20$Categoria_empresa <- fct_relevel(ind_2_20$Categoria_empresa, 
                                          "Micro", "Pequeña", "Mediana T1", "Mediana T2", "Grande", "Sin dato") 

ind_2_20$NIVEL_ED <- fct_relevel(ind_2_20$NIVEL_ED, 
                                 "Sin instrucción", "Prim. incompleta", "Prim. completa", "Sec. incompleta", "Sec. completa",
                                 "Univ. incompleta", "Univ. completa") 

levels(ind_2_20$Categoria_empresa)

####Resultados####

#TIPO DE ORGANIZACIÓN

tabla1 <- calculate_tabulates(base=ind_2_20, x='Tipo_Organizacion', y='Tipo_Ocupacion', weights = 'PONDERA',
                              add.totals='row', add.percentage='row')
tabla1 %>%
  kbl() %>%
  kable_paper("hover", full_width = T, position = "left")


#GÉNERO

tabla2 <- calculate_tabulates(base=ind_2_20, x='Genero', y='Tipo_Ocupacion', weights = 'PONDERA',
                              add.totals='row', add.percentage='row') 
tabla2 %>%
  kbl() %>%
  kable_paper("hover", full_width = T, position = "left")


#INFORMALIDAD POR NIVEL EDUCATIVO

tabla3 <- ind_2_20 %>%  
  filter(P21>0) %>%
  group_by(NIVEL_ED) %>% 
  summarise(Formales        = sum(PONDERA[Ocupado_inf == 0]),
            Informales      = sum(PONDERA[Ocupado_inf == 1]),
            Total           = sum(PONDERA),
            Tasa_Informales = Informales/Total)

ggplot(data = tabla3) +
  geom_col(mapping = aes(x=NIVEL_ED, y=Tasa_Informales)) +
  ggtitle ("Tasa de informalidad por nivel educativo") +
  theme(legend.position="bottom")

#INFORMALIDAD POR TIPO DE EMPRESA 1

tabla4 <- ind_2_20 %>% 
  filter(Tipo_Organizacion == "Privada", P21>0) %>% 
  group_by(Categoria_empresa) %>% 
  summarise(Formales        = sum(PONDERA[Ocupado_inf == 0]),
            Informales      = sum(PONDERA[Ocupado_inf == 1]),
            Total           = sum(PONDERA),
            Tasa_Informales = Informales/Total)

ggplot(data = tabla4) +
  geom_col(mapping = aes(x=Categoria_empresa, y=Tasa_Informales)) +
  ggtitle ("Tasa de informalidad por categoría de empresa") +
  theme(legend.position="bottom")


#INFORMALIDAD POR TIPO DE EMPRESA 2

tabla5 <- ind_2_20 %>% filter(Tipo_Organizacion == "Privada") %>% 
  filter(P21>0) %>% 
  group_by(Categoria_empresa, NIVEL_ED) %>% 
  summarise(Formales        = sum(PONDERA[Ocupado_inf == 0]),
            Informales      = sum(PONDERA[Ocupado_inf == 1]),
            Total           = sum(PONDERA))

ggplot(data = tabla5) +
  geom_col(mapping = aes(x=Categoria_empresa, y=Total, fill = NIVEL_ED)) +
  ggtitle ("Cantidad de ocupados por nivel educativo") +
  scale_y_continuous(limits = c(0, 2000000)) +
  theme(legend.position="bottom")

tabla6 <- tabla5 %>% filter(Categoria_empresa != "Sin dato")

treemap(tabla6,
        index=c("Categoria_empresa","NIVEL_ED"),
        vSize="Total",
        type="index")


#INFORMALIDAD POR DECIL DE INGRESO SECTOR PRIVADO

tabla7 <- ind_2_20 %>% filter(Tipo_Organizacion == "Privada") %>% 
  filter(P21>0) %>% 
  group_by(DECOCUR) %>% 
  summarise(Formales        = sum(PONDIIO[Ocupado_inf == 0]),
            Informales      = sum(PONDIIO[Ocupado_inf == 1]),
            Total           = sum(PONDIIO),
            Tasa_Informales = Informales/Total)

tabla7 <- tabla7 %>% arrange(DECOCUR)


ggplot(data = tabla7) +
  geom_col(mapping = aes(x=DECOCUR, y=Tasa_Informales)) +
  ggtitle ("Tasa de informalidad por decil de ingresos sector privado") +
  theme(legend.position="bottom")


#INFORMALIDAD POR DECIL DE INGRESO SECTOR PÚBLICO

tabla8 <- ind_2_20 %>% filter(Tipo_Organizacion == "Publica") %>% 
  filter(P21>0) %>% 
  group_by(DECOCUR) %>% 
  summarise(Formales        = sum(PONDIIO[Ocupado_inf == 0]),
            Informales      = sum(PONDIIO[Ocupado_inf == 1]),
            Total           = sum(PONDIIO),
            Tasa_Informales = Informales/Total)

tabla8 <- tabla8 %>% arrange(DECOCUR)


ggplot(data = tabla8) +
  geom_col(mapping = aes(x=DECOCUR, y=Tasa_Informales)) +
  ggtitle ("Tasa de informalidad por decil de ingresos sector público") +
  theme(legend.position="bottom")
