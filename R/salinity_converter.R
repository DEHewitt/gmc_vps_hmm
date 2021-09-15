salinity_converter <- function(data){
  # convert conductivity to salinity
  # Salinity is calculated from conductivity according to the UNSECO formulae described in 
  # Fofonoff, P. and Millard, R.C. (1983) Algorithms for computation of fundamental 
  # properties of seawater, Unesco Technical Papers in Marine Sci., 44, 58 pp.
  
  # set constants
  c0 =  0.6766097
  c1 =  2.00564e-2
  c2 =  1.104259e-4
  c3 =  -6.9698e-7
  c4 =  1.0031e-9
  d1 = 3.426e-2
  d2 = 4.464e-4
  d3 = 4.215e-1
  d4 = -3.107e-3
  e1 = 2.070e-5
  e2 = -6.370e-10
  e3 = 3.989e-15
  a0 = 0.0080
  a1 = -0.1692
  a2 = 25.3851
  a3 = 14.0941
  a4 = -7.0261
  a5 = 2.7081
  b0 =  0.0005
  b1 = -0.0056
  b2 = -0.0066
  b3 = -0.0375
  b4 =  0.0636
  b5 = -0.0144
  k  =  0.0162
  
  # water pressure (in decibars) at 0.4m depth (approx. deployment depth)
  P = 10.5346 
  
  data <- data %>%
    mutate(RT35 = (((c3+c4*temp)*temp+c2)*temp+c1)*temp+c0) %>%
    mutate(cnd = cond/42.914) %>%
    mutate(R = cnd) %>%
    mutate(RP = 1+(P*(e1+e2*P+(e3*P^2)))/(1+(d1*temp)+(d2*temp^2)+(d3+(d4*temp))*R)) %>%
    mutate(RT = R/(RP*RT35)) %>%
    mutate(XR = sqrt(RT)) %>%
    mutate(XT = temp-15) %>%
    mutate(DSAL = (XT/(1+k*XT))*(b0+(b1+(b2+(b3+(b4+(b5*XR))*XR)*XR)*XR)*XR)) %>%
    mutate(SAL = (((((a5*XR)+a4)*XR+a3)*XR+a2)*XR+a1)*XR+a0) %>%
    mutate(sal = SAL + DSAL) %>%
    dplyr::select(-RT35, -cnd, -R, -RP, -RT, -XR, -XT, -DSAL, -SAL)
}
