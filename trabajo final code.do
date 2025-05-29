
***************************************
*****  Trabajo final Econometria AV **
****************************************
*ssc install rdrobust, replace
*ssc install rddensity, replace
*ssc install lpdensity, replace


use "C:\Users\HP\Desktop\Econometría avanzada 2025-1\proyecto final\censo_margin_vacunasv2.dta", clear
 * Outcome de interés (Y):  GenWin - vacunacion
	* Variable indep. de interés (T): primary winner - dem_won
	* Running variable (Z) X: Margen de votos con la que ganó (o perdió) el partido, margin_pct
	
	

 
  * Acá verificamos que es RD nítido: 
  
/* Esto nos indica que estamos en el mundo de regresión discontinua nítida 
 	   y que la asignación al tratamiento corresponde exactamente a la 
	   condición de tratamiento efectiva.
	   */
twoway (scatter dem_won margin_pct if margin_pct < 0, mfcolor(gray%80*0.7) mlcolor(gray%80)) /// 
       (scatter dem_won margin_pct if margin_pct >= 0, mfcolor(gray%80*0.7) mlcolor(gray%80)) /// 
       (qfit dem_won margin_pct if margin_pct < 0, lcolor(black)) /// 
       (qfit dem_won margin_pct if margin_pct >= 0, lcolor(black)), /// 
       xline(0, lcolor(black)) graphregion(fcolor(white)) legend(off) /// 
       xtitle("Margen de votos con que ganaron (+) o perdieron (-) los democrátas") ytitle("Probabilidad de ser tratado") ///
       title("Discontinuidad en la probabilidad de tratamiento") ///
       yscale(range(0 1)) /// 
       ylabel(0(0.2)1)

graph export "RDnitido.png", replace   


local vars administered_dose1_pop_pct ///
           administered_dose1_recip_12plusp ///
           administered_dose1_recip_18plusp ///
           administered_dose1_recip_65plusp ///
           series_complete_pop_pct ///
           series_complete_12pluspop_pct ///
           series_complete_18pluspop_pct ///
           series_complete_65pluspop_pct

foreach v of local vars {

    // Convertir a numérica (si es string), ignorando errores si ya es numérica
    capture destring `v', replace ignore(".")

    // Intentar hacer rdplot, si da error por falta de observaciones, mostrar advertencia
    
       rdplot `v' margin_pct, c(0) p(2) ///
    graph_options( ///
        xtitle("Margen de votos con que ganó (+) o perdió (-)", size(small)) ///
        ytitle("Tasa vacunación", size(small)) ///
        title("Discontinuidad en la probabilidad de tratamiento: `v'", size(small)) ///
        graphregion(fcolor(white)) ///
        legend(rows(2) size(small) ring(0) position(1)) ///
    ) ///
    dot_options(mcolor(gray%80)) ///
    line_options(lcolor(black))

graph export "rdplot_`v'.png", replace

   

}





*) Descripción de RDN
ssc install rdrobust, replace
ssc install rddensity, replace
ssc install lpdensity, replace

destring vacunacion, replace

*) Verificación de supuestos	// ojo esto revisarlooooooooooooo	y cambiar nombres
global controles pct_adults_65plus pct_bachelor_degree pct_uninsured pct_healthcare_employed

	* 1. Continuidad local
	* Aproximación gráfica

	foreach var of global controles {
    rdplot `var' margin_pct, p(2) binselect("es") ///
        graph_options(xtitle("Margen de Victoria de los Demócratas") ///
                      ytitle("`var'") ///
                      title("Continuidad local - `var'") ///
                      name(graph_`var', replace) graphregion(fcolor(white)) legend(off))
    graph export "graph_`var'.png", replace

    di "Ttest para `var' en ventanas diferentes"

    ttest `var' if inrange(margin_pct, -0.5, 0.5), by(dem_won)
    ttest `var' if inrange(margin_pct, -0.25, 0.25), by(dem_won)
    ttest `var' if inrange(margin_pct, -0.1, 0.1), by(dem_won)
}

	
	
	
	* 2. No manipulación
	

	kdensity margin_pct, xline(0,lcol(red) lp("--")) lcol(black)	
	
	rddensity margin_pct
	rddensity margin_pct, plot ///
    graph_opt(                                                    ///
        title("Test de manipulación")                             ///
        xtitle("Margen de Victoria de los Demócratas")            ///
        ytitle("Densidad de la variable de focalización")         ///
        scheme(s1mono) legend(off) graphregion(fcolor(white))     ///
        xline(0, lpattern(dot) lcolor(black))                     ///
    )

		
			
*) Estimaciones


* Bucle para aplicar rdrobust a cada variable
* Polinomio grado uno diferente a ambos lados alrededor del corte
	   

use "C:\Users\HP\Desktop\Econometría avanzada 2025-1\proyecto final\censo_margin_vacunasv2.dta", clear

gen T_X=margin_pct*dem_won
gen X2=margin_pct^2
gen T_X2=X2*dem_won

keep if  dem_won2016 == dem_won


		   
global controles pct_adults_65plus pct_bachelor_degree pct_uninsured ///
                pct_healthcare_employed white_pct black_pct asian_pct hispanic_pct

				
****resultados oficiales

* Lista de variables de vacunación
local vars administered_dose1_pop_pct 


foreach vacunacion of local vars {
	capture destring `v', replace ignore(".")
    
	
    di "-----------------------------------------------------"
    di "Estimación parametrica para `vacunacion' - Polinomio grado 2, insesgado, errores robustos"
    reg `vacunacion'  dem_won margin_pct X2 T_X T_X2 $controles if inrange(margin_pct,-5,5),  r
	
 

******************************************
**resultados con otros polinomios y bandwiths
********************************************

use "C:\Users\HP\Desktop\Econometría avanzada 2025-1\proyecto final\censo_margin_vacunasv2.dta", clear

gen T_X=margin_pct*dem_won
gen X2=margin_pct^2
gen T_X2=X2*dem_won

keep if  dem_won2016 == dem_won


		   
global controles pct_adults_65plus pct_bachelor_degree pct_uninsured ///
                pct_healthcare_employed white_pct black_pct asian_pct hispanic_pct

				
****resultados oficiales

* Lista de variables de vacunación
local vars administered_dose1_pop_pct 


foreach vacunacion of local vars {
	capture destring `v', replace ignore(".")
    
	
    di "-----------------------------------------------------"
    di "Estimación parametrica para `vacunacion' - Polinomio grado 2, insesgado, errores robustos"
    reg `vacunacion'  dem_won margin_pct X2 T_X T_X2 $controles if inrange(margin_pct,-7.5,7.5),  r
	
 
	
    di "-----------------------------------------------------"
    di "Estimación parametrica para `vacunacion' - Polinomio grado 2, insesgado, errores robustos"
    reg `vacunacion'  dem_won margin_pct X2 T_X T_X2 $controles if inrange(margin_pct,-3, 3),  r
	
	
	 
	
    di "-----------------------------------------------------"
    di "Estimación parametrica para `vacunacion' - Polinomio grado 2, insesgado, errores robustos"
    reg `vacunacion'  dem_won margin_pct X2 T_X T_X2 $controles if inrange(margin_pct,-1.5,1.5),  r
	
	
}


**********************************
**resultados con otras medidas
**********************************

use "C:\Users\HP\Desktop\Econometría avanzada 2025-1\proyecto final\censo_margin_vacunasv2.dta", clear

gen T_X=margin_pct*dem_won
gen X2=margin_pct^2
gen T_X2=X2*dem_won


keep if dem_won2016 == dem_won 

		   
global controles pct_adults_65plus pct_bachelor_degree pct_uninsured ///
                pct_healthcare_employed white_pct black_pct asian_pct hispanic_pct

* Lista de variables de vacunación
local vars administered_dose1_pop_pct administered_dose1_recip_12plusp ///
           administered_dose1_recip_18plusp ///
           administered_dose1_recip_65plusp ///
           series_complete_pop_pct ///
           series_complete_12pluspop_pct ///
           series_complete_18pluspop_pct ///
           series_complete_65pluspop_pct


foreach vacunacion of local vars {
	capture destring `v', replace ignore(".")
    
	
    di "-----------------------------------------------------"
    di "Estimación parametrica para `vacunacion' - Polinomio grado 2, insesgado, errores robustos"
    reg `vacunacion'  dem_won margin_pct X2 T_X T_X2 $controles if inrange(margin_pct,-5,5),  r
	
 
	
	
}