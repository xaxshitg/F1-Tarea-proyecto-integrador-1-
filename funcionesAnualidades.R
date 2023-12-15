  #Funciones para Valor Futuro conocido: Anualidades vencidas 
  
  ##Primera función valor futuro para a vencidas
  
  VFVen= function(anualidad, tasa, periodos)
  {
  vf= anualidad *((1+tasa)^periodos-1)/ tasa
  return(vf)
  }
  print("Ingrese el pago anual:")
  anualidad= scan(n=1)
  print("Ingrese la tasa de interés:")
  tasa= scan(n=1)
  print("Ingrese el número de periodos:")
  periodos= scan(n=1)
  
  resultado= VFVen(anualidad, tasa, periodos)
  print(paste("El valor futuro es: $", resultado))
  
  ##Segunda función anualidad para a vencidas
  
  pagoVFVen= function(valorf, periodos1, tasa1)
  {
  a= valorf/(((1+tasa1)^periodos1-1)/tasa1)
  return(a)
  }
  print("Ingrese el valor futuro conocido:")
  valorf= scan(n=1)
  print("Ingrese tasa de interés:")
  tasa1= scan(n=1)
  print("Ingrese el número de periodos:")
  periodos1= scan(n=1)
  
  pago= pagoVFVen(valorf, periodos1, tasa1)
  print(paste("El pago deberá ser de: $", pago))
  
  ##Tercera función para encontrar el número de periodos para a vencidas
  
  periodosVFVen= function(valorf1, anualidad1, tasa2)
  {
  per= log(((valorf1*tasa2)/anualidad1)+1)/log(1+tasa2)
  return(per)
  }
  print("Ingrese el valor futuro:")
  valorf1= scan(n=1)
  print("Ingrese el pago anual:")
  anualidad1= scan(n=1)
  print("Ingrese la tasa de interés:")
  tasa2= scan(n=1)
  
  nper= periodosVFVen(valorf1, anualidad1, tasa2)
  print(paste("El número de periodos es:", nper))
  
  ##Cuarta función para encontrar la tasa de interes para a vencidas
  
  tasaVFVen = function(vf, t, a)
  {
    dif = 1000000
    aprox = 0
    
    for (i in 1:1000000) {
      r = i / 1000000
      aprox = a *((1 + r)^ t - 1)/r
      dif = abs(vf - aprox)
      
      if (vf / 10000 >= dif) 
      {
        break
      }
    }
    
    return(r * 100 * 24)
  }
  print("Ingrese valor futuro:")
  vfo= scan(n=1)
  print("Ingrese el pago anual:")
  a= scan(n=1)
  print("Ingrese el número de periodos:")
  t= scan(n=1)
  
  tasav= tasaVFVen(vfo, a, t)
  print(paste("La tasa encontrada es:", round(tasav, digits=2)))
  
  
  #Funciones para Valor Actual conocido: Anualidades vencidas
  
  ##Primera función valor actual para a vencidas
  
  VAVen= function(anualidad2, periodos2, tasa3)
  {
  va= anualidad2*((1-(1+tasa3)^-periodos2)/tasa3)
  return(va)
  }
  print("Ingrese la anualidad:")
  anualidad2= scan(n=1)
  print("Ingrese los periodos:")
  periodos2= scan(n=1)
  print("Ingrese tasa de interés:")
  tasa3= scan(n=1)
  
  vat= VAVen(anualidad2, periodos2, tasa3)
  print(paste("Este es el valor presente: $", vat))
  
  ##Segunda función valor actual para a vencidas
  
  pagoVAVen= function(valora, tasa4, periodos3)
  {
  a= valora*tasa4/((1-(1+tasa4)^-periodos3))
  return(a)
  }
  print("Ingrese el valor presente:")
  valora= scan(n=1)
  print("Ingrese la tasa de interés:")
  tasa4= scan(n=1)
  print("Ingrese los periodos:")
  periodos3= scan(n=1)
  
  at= pagoVAVen(valora, tasa4, periodos3)
  print(paste("La anualidad para el valor presente: $", at))
  
  ##Tercera función para encontrar el numero de periodos para a vencidas
  
  periodosVAVen= function(valora1, anualidad3, tasa5)
  {
  tiempo= -log(1-((valora1*tasa5)/anualidad3))/log(1+tasa5)
  return(tiempo)
  }
  print("Ingrese el valor presente:")
  valora1= scan(n=1)
  print("Ingrese tasa:")
  tasa5= scan(n=1)
  print("Ingrese pago anual:")
  anualidad3= scan(n=1)
  
  perva= periodosVAVen(valora1, anualidad3, tasa5)
  print(paste("Los periodos para valor presente conocido son:", perva))
  
  ##Cuarta función para encontrar tasa de interés a vencidas
  
  tasaVAVen = function(va, t2, a2)
    {
    dif = 1000000
    aprox = 0
    
    for (i in 1:1000000) 
      {
      r3 = i / 1000000
      aprox = a2 * (1 - (1 + r3) ^ -t2) / r3
      dif = abs(va - aprox)
      
      if (va / 10000 >= dif) 
      {
        break
      }
    }
    
    return(r3 * 100 * 24)
  }
  print("Ingrese el valor presente:")
  va= scan(n=1)
  print("Ingrese el número de plazos:")
  t2= scan(n=1)
  print("Ingrese el pago anual:")
  a2= scan(n=1)
  
  tasava= tasaVAVen(va, t2, a2)
  print(paste("La tasa de interés es del:", tasava))
  
  
  #Funciones para Anualidades Anticipadas: valor futuro
  
  ##Primera función para valor futuro a anticipadas
  
  VFAnt= function(panual, interes, plazos)
  {
  vfan= (panual*(((1+interes)^plazos-1)/interes))*(1+interes)
  return(vfan)
  }
  print("Proporcione pago anual:")
  panual= scan(n=1)
  print("Proporcione tarifa de interés:")
  interes= scan(n=1)
  print("Proporcione el número de plazos:")
  plazos= scan(n=1)
  
  valorfuturoa= VFAnt(panual, interes, plazos)
  print(paste("Este es el valor futuro para anualidad anticipada: $", valorfuturoa))
  
  ##Segunda función para pago anual a anticipadas
  
  pagoVFAnt= function(vfuturo, interes1, plazos1)
  {
  pagoa= vfuturo*interes1/((((1+interes1)^plazos1-1))*(1+interes1))
  return(pagoa)
  }
  print("Proporcione valor futuro de anualidad anticipada:")
  vfuturo= scan(n=1)
  print("Proporcione tarifa de interes:")
  interes1= scan(n=1)
  print("Proporcione el número de plazos:")
  plazos1= scan(n=1)
  
  pagoanual= pagoVFAnt(vfuturo, interes1, plazos1)
  print(paste("El pago anual de la anualidad anticipada es: $", pagoanual))
  
  ##Tercera función para número de plazos anualidad anticipada
  
  periodosVFAnt= function(vfuturo1, panual1, interes2)
  {
  npla= log(1+(vfuturo1/(panual1*(1+interes2)))*interes2)/log(1+interes2)
  return(npla)
  }
  print("Proporcione el valor futuro de anualidad anticipada:")
  vfuturo1= scan(n=1)
  print("Proporcione pago anual:")
  panual1= scan(n=1)
  print("Poroporcione tarifa de interés:")
  interes2= scan(n=1)
  
  plazosn= periodosVFAnt(vfuturo1, panual1, interes2)
  print(paste("El número de plazos es:", plazosn))
  
  ##Cuarta función para encontrar la tasa de interés
  
  tasaVFAnt = function(vf1, t1, a1)
  {
    dif = 1000000
    aprox = 0
    
    for (i in 1:1000000) 
    {
      r1 = i / 1000000
      aprox = a1 * ((1 + r1)^ t1 - 1) / r1 * (1 + r1)
      dif = abs(vf1 - aprox)
      
      if (vf1 / 10000 >= dif) 
      {
        break
      }
    }
    
    return(r1 * 100 * 24)
  }
  print("Proporcione el valor futuro:")
  vf1= scan(n=1)
  print("Proporcione el pago:")
  a1= scan(n=1)
  print("Proporcione el número de plazos:")
  t1= scan(n=1)
  
  tarifai= tasaVFAnt(vf1, t1, a1)
  print(paste("La tarifa de interés es:", tarifai))
  
  #Funciones para Anualidades Anticipadas: valor presente
  
  ##Primera función valor presente
  
  VAAnt= function(panual2, plazos2, interes3)
  {
  vp= (panual2*((1-(1+interes3)^-plazos2)/interes3))*(1+interes3)
  return(vp)
  }
  print("Proporcione el pago anual:")
  panual2= scan(n=1)
  print("Proporcione el número de plazos:")
  plazos2= scan(n=1)
  print("Proporcione la tarifa de interés:")
  interes3= scan(n=1)
  
  vpresente= VAAnt(panual2, plazos2, interes3)
  print(paste("El valor presente es: $", vpresente))
  
  ##Segunda función pago anual vp
  
  pagoVAAnt= function(vpre, interes4, plazos3)
  {
  pagoa= vpre/(((1-(1+interes4)^-plazos3)/interes4)*(1+interes4))
  return(pagoa)
  }
  print("Proporcione el valor presente:")
  vpre= scan(n=1)
  print("Proporcione tarifa de interés:")
  interes4= scan(n=1)
  print("Proporcione número de plazos:")
  plazos3= scan(n=1)
  
  paganual= pagoVAAnt(vpre, interes4, plazos3)
  print(paste("Este es el pago anual: $", paganual))
  
  ##Tercera función número de plazos vp
  
  periodosVAAnt= function(vpre1, panual3, interes5)
  {
  npla= -log(1-(vpre1/(panual3*(1+interes5))*interes5))/log(1+interes5)
  return(npla)
  }
  print("Proporcione el valor actual:")
  vpre1= scan(n=1)
  print("Proporcione el pago anual:")
  panual3= scan(n=1)
  print("Proporcione tarifa de interés:")
  interes5= scan(n=1)
  
  nplazos= periodosVAAnt(vpre1, panual3, interes5)
  print(paste("El número de plazos es:", nplazos))
  
 ##Cuarta función para encontrar la tasa de interés 
  
  tasaVAAnt = function(va1, t3, a3)
  {
    dif = 1000000
    aprox = 0
    
    for (i in 1:1000000) 
    {
      ri = i / 1000000
      aprox = a3 * (1 - (1 + ri)^ -t3) / ri * (1 + ri)
      dif = abs(va1 - aprox)
      
      if (va1 / 10000 >= dif) 
      {
        break
      }
    }
    
    return(ri * 100 * 24)
  }
  print("Proporcione valor presente:")
  va1= scan(n=1)
  print("Proporcione número de plazos:")
  t3= scan(n=1)
  print("Proporcione pago anual:")
  a3= scan(n=1)
  
  tarif= tasaVAAnt(va1, t3, a3)
  print(paste("La tarifa de interés es:", tarif))
  
  #Funciones para anualidades diferidas: con pagos vencidos
  
  ##Primera función para encontrar valor presente
  
  VADif = function(pago, tasaInteres, plazo, diferimiento)
  {
    vadif= (pago*((1-(1+tasaInteres)^-plazo))/tasaInteres)/(1+tasaInteres)^diferimiento
    return(vadif)
  }
  print("Ponga el pago:")
  pago= scan(n=1)
  print("Ponga la tasa de interés:")
  tasaInteres= scan(n=1)
  print("Ponga el número de plazos:")
  plazo= scan(n=1)
  print("Ponga la diferencia del plazo original:")
  diferimiento= scan(n=1)
  
  valadif= VADif(pago, tasaInteres, plazo, diferimiento)
  print(paste("El préstamo será ahora de:", valadif))
  
  ##Segunda función para encontrar el pago:
  pagoVADif = function(m, ta, pla, dif) 
  {
    tasaq = (1+ta)^(1/12)-1
    nump = pla * 12
    pagoq = m/((1-(1+tasaq)^-nump)/tasaq)*(1+tasaq)
    
    return(pagoq)
  }
  print("Ponga la tasa de interés:")
  ta= scan(n=1)
  print("Ponga el número de plazos:")
  pla= scan(n=1)
  pagoqt = pagoVADif(monto_prestamo_total, ta, pla )
  print("Ponga el diferimiento de plazo:")
  dif= scan(n=1)
  pagoqdif = pagoQuincenal(monto_prestamo_diferido, ta, pla, dif)
  
  print(paste("Pago anual es:", pagoqdif))
  
  