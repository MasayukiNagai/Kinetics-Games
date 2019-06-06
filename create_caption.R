create_caption = function(mechanism = "monomolecular"){
  if(mechanism == "monomolecular"){
    caption = includeHTML("captions/monomolecular.html")
  } else if(mechanism == "bimolecular"){
    caption = includeHTML("captions/bimolecular.html")
  } else if(mechanism == "catalytic"){
    caption = includeHTML("captions/catalytic.html")
  } else if(mechanism == "autocatalytic"){
    caption = includeHTML("captions/autocatalytic.html")
  } else if(mechanism == "consecutive"){
    caption = includeHTML("captions/consecutive.html")
  } else {
    caption = includeHTML("captions/equilibrium.html")
  }
  
  return(caption)
}