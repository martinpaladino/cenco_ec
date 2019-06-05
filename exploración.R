base_cortos <- ics

base_cortos %>% 
  select(NBI, publico, pea, pobtot, canton) %>% 
  mutate(prop_pea = publico/pea, prop_tot = publico/pobtot) %>% 
  select(-pobtot, -pea, -publico) %>% 
  gather(variable, valor, -NBI, -canton) %>% 
  ggplot(aes(x=NBI, y = valor)) + geom_point() + 
  geom_smooth() + 
  geom_smooth(method = "lm", color = "red") +
  facet_wrap(.~variable, scales = "free")


base_cortos %>% 
  select(NBI, publico, pea, pobtot, canton) %>% 
  mutate(prop_pea = publico/pea, prop_tot = publico/pobtot) %>% 
  select(NBI, prop_tot, canton) %>% 
  lm(NBI~prop_tot, data = .) %>% summary
  
base_cortos %>% 
  select(NBI, publico, pea, pobtot, canton, provincia) %>% 
  mutate(prop_pea = publico/pea, prop_tot = publico/pobtot) %>% 
  #filter(prop_tot <= 0.1) %>% 
  ggplot(aes(y=NBI, x = prop_pea, label = paste(canton, "-", provincia))) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.1) + 
  theme_minimal() +  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_size(range = c(1,6)) +
  labs(x = "Población empleada en el sector público como porcentaje de la población total", 
       y = "Pocentaje de población con Necesidades Básica Insatisfechas")


base_cortos %>% 
  filter(provincia != "GALÁPAGOS") %>% 
  mutate(prop_pea = publico/pea, prop_tot = publico/pobtot) %>% #summarise(cor(prop_tot, NBI, method = "spearman"))
  ggplot(aes(y=NBI, x = prop_tot, label = str_to_title(paste(canton, provincia)))) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  stat_dens2d_filter(geom = "text_repel", keep.fraction = 0.1) + 
  theme_minimal() +  
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_size(range = c(1,6)) +
  labs(x = "Población empleada en el sector público como porcentaje de la población total", 
       y = "Pocentaje de población con Necesidades Básica Insatisfechas")

base_cortos %>% 
  mutate(prop_pea = publico/pea, prop_tot = (publico/pobtot)*100) %>%

  lm(NBI~prop_tot, data = .) %>% summary

base_cortos %>% 
  mutate(prop_pea = publico/pea, prop_tot = (publico/pobtot)*100) %>% 
  lm(NBI~prop_tot, data = .) -> moli2


# Ajuste de modelos lasso

library(broom)
library(glmnet)

grafico_lasso <- function(fit, cvfit){
  
  #data.frame con el valor de la norma l1 para cada valores óptimos de lambda. 
  # glance() extrae por criterio min y 1.se
  
  normas <- map_df(glance(cvfit), ~sum(abs(coef(cvfit, s= .x, exact = TRUE)[-1])))
  
  tidy(fit, return_zeros = TRUE) %>% 
    filter(term != "(Intercept)") %>%                     #Elimino ordenada al origen, no está penalizada 
    group_by(lambda) %>%
    mutate(s = sum(abs(estimate))) -> fit
  
  ggplot(fit) +
    geom_line(aes(x=s, y = estimate, color = term)) + 
    geom_rect(data = normas, aes(xmin=lambda.1se, xmax=lambda.min, ymin=-Inf, ymax=Inf), 
              alpha = 0.1, fill = "purple") + #Zona pintada (no es necesario)
    geom_vline(data = gather(normas, Criterio, value),
               aes(xintercept = value, 
                   linetype = Criterio)) + 
    geom_text(data=fit %>% 
                group_by(term) %>% 
                top_n(1, abs(estimate)),
              aes(x = s + 0.03, label=term, y = estimate), hjust=0)
}


ics %>% 
  select(pobtot:publico) %>% 
  mutate(activa = pea/pobtot, 
         desocupada = ((pea-ocupada)/pea)*100, 
         prop_publico = (publico/pea)*100) %>%
  select(-activa, -pea, -ocupada, -publico, - pobtot, -otra) %>% 
  select(NBI, everything()) -> datos
  as.matrix() -> ics_limpio

y <- ics_limpio [ , 1]  
X <- ics_limpio [,-1]

lasso <- glmnet(X, y)
cv_lasso <- cv.glmnet(X, y)


grafico_lasso(lasso, cv_lasso)
cbind(coef(lasso, cv_lasso$lambda.min), coef(lasso, cv_lasso$lambda.1se))

lambda <- cv_lasso$lambda.1se
beta = coef(lasso, s=lambda/nrow(ics_limpio), exact=TRUE,x=X,y=y)[-1]
fixedLassoInf(X,y,beta,lambda)