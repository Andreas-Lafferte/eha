

a <- hombres_b %>% 
  filter(edad != "100 and over") %>% 
  reframe(t, hb = nmx)

b <- mujeres_b %>% 
  filter(edad != "100 and over") %>% 
  reframe(t, mb = nmx)

full_join(a, b) %>% 
  rowwise() %>% 
  mutate(ratio = hb/mb) %>% 
  ungroup() %>% 
  ggplot(aes(x = t, y = ratio, group = 1)) +
  geom_line()


c <- hombres_n %>% 
  filter(edad != "100 and over") %>% 
  reframe(t, hn = nmx)

d <- mujeres_n %>% 
  filter(edad != "100 and over") %>% 
  reframe(t, mn = nmx)

full_join(c, d) %>% 
  rowwise() %>% 
  mutate(ratio = hn/mn) %>% 
  ungroup() %>% 
  ggplot(aes(x = t, y = ratio, group = 1)) +
  geom_line()



datos <- data.frame(edad = hombres_b$edad,
                    t = hombres_b$t,
                    nmx_hombres_blancos = hombres_b$nmx,
                    nmx_hombres_negros = hombres_n$nmx,
                    nmx_mujeres_blancas = mujeres_b$nmx,
                    nmx_mujeres_negras = mujeres_n$nmx)

mean(datos$nmx_mujeres_negras)
