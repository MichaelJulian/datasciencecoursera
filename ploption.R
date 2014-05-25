# Option Plotter:
# Ploption


require(RQuantLib)
require(ggplot2)
require(scales)


ploption <- function(cp, underlying, strike, div, rf, dte, vol, premium)
{
    # Set parameters for the x axis :: 25% above and below at-the-money
    xaxis <- seq(underlying*.75, underlying*1.25, underlying*.005)
    
    # Create P&L vectors: Today, Expiry, and halfway 'til expiry
    pnl <- vector()
    pnl2 <- vector()
    pnl3 <- vector()
    
    # Iterate over all stock prices, incremented by the sequence (underlying*.005)
    # Enter 
    for (i in xaxis)
    {
        pnl <- c(pnl, AmericanOption(cp, i, strike, div, rf, dte/365, vol)$value - premium)
        pnl2 <- c(pnl2,AmericanOption(cp, i, strike, div, rf, dte/730, vol)$value - premium)
        pnl3 <- c(pnl3, AmericanOption(cp, i, strike, div, rf, 1/365, vol)$value - premium)
    }
    
    
    # Profit and Loss Statistics
    print(paste0("Max Profit at Expiry: $", max(pnl3)*100))
    print(paste0("Max Loss at Expiry: $", min(pnl3)*100))
    
    
    qplot(xaxis, pnl3) + geom_point(aes(y=pnl, color=pnl), alpha=.8) +
        geom_point(aes(y=pnl2, color=pnl2), alpha=.9) + 
        geom_point(aes(y=pnl3, color=pnl3)) + 
        scale_colour_gradient2(limits=c(min(pnl3),max(pnl)), low="red", mid="white", high=muted("green"))
     
}

#ploption('call', 50, 52.5, .001, .001, 65, .35, 2.2)
