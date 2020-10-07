indexes.list <- list(
  SP500 = list(url = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies",
               appendum = NULL,
               Ticker = "Symbol",
               Company = "Security", 
               componentNode = '//*[@id="constituents"]'),
  
  # MIB: 40 components
  MIB = list(url = "https://en.wikipedia.org/wiki/FTSE_MIB",
             appendum = ".MI",
             Ticker = "Ticker",
             Company = "Company", 
             componentNode = '//*[@id="constituents"]'),
  

  # DAX: 30 components
  DAX = list(YFticker = "%5EGDAXI"), 

  # MDAX: 50 Components
  MDAX = list(YFticker = "%5EMDAXI"),
  
  # CAC: 40 components
  CAC = list(url = "https://en.wikipedia.org/wiki/CAC_40",
             appendum = NULL, # Already added
             Ticker = "Ticker",
             Company = "Company", 
             componentNode = '//*[@id="constituents"]'),
  
  # CAC next 20
  CACn20 = list(YFticker = "%5ECN20"),
  
  # IBEX: 35 components
  IBEX = list(url = "https://en.wikipedia.org/wiki/IBEX_35",
              appendum = ".MC",
              Ticker = "Ticker",
              Company = "Company", 
              componentNode = '//*[@id="mw-content-text"]/div[1]/table[2]'),
  
  AEX = list(YFticker = "%5EAEX"),
  AMX = list(YFticker = "%5EAMX"),
  
  EUROSTOXX50 = list(url = "https://en.wikipedia.org/wiki/EURO_STOXX_50",
                     appendum = NULL, # Already added
                     Ticker = "Ticker",
                     Company = "Name", 
                     componentNode = '//*[@id="mw-content-text"]/div[1]/table[3]')
)

