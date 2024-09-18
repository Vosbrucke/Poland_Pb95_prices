# Analysis of price-setting factors impacting Pb95 prices in Poland during 2022 market turmoil

Price of gasoil is an aggregate of a number of different factors contribuing to the final price we, as consumers, find on gas stations. It therefore presents itself stable when the market is not disrupted by internal and/or external factors and is a subject to change once situation shifts. This has been very visible starting from the year 2020 when global COVID pandemic impacted all the aspects of life sending ripples onto economy. Every country experienced its effects on its own way. In 2022 when it seemed the pandemic was taken under control we saw yet another event shaking the grounds of global market- Russian invasion on Ukraine. In response European Union imposed sanctions on Russia with quotas on oil extracted on Russian soil. Under the agreement, Member States were required to limit buying Russian oil and find a different resource. The abrupt nature of the event, the pace counter measures were implemeted caused major disruptions on the oil market prices among various European countries. One such situation occured in Poland which saw an unprecedented before swings in Pb95 oil. As the economy has just barely moved out of one groundbraking event, this has only added further to the economic grievances so much present in the polish society. The topic of rising costs of life, running business instantly came back turning into the main talking point across society and medias. A particular view on the issue was created, propagated and widely accepted- that the visible price of Pb95 on gas stations is a result of a greediness of domestically owned fuel company Orlen (and Lotos). The goal of the project is to dissect this notion and provide an answer to what the underlying factors behind the market price development were.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Brent%20in%20USD%20vs%20brent%20in%20PLN.png" width="550">
</p>

## Introduction

The price of fuel observed at gas stations depends on a number of different factors. Recent years have been characterized by significant fluctuations on the global market, which is reflected in fuel prices. The first problems were caused by the COVID-19 pandemic - restrictions on trade, movement, quarantines. However, the specter of recession was quickly eliminated by the decisive response of most governments and central banks around the world. However, the economic recovery caused by expansionary economic policy soon led to further shocks. Producers trying to keep up with demand increased their production capacity, contributing to the increase in demand for crude oil. Already in February 2021, the price of oil returned to pre-pandemic levels, reaching USD 60 per barrel. In the following months, the increase continued due to problems in increasing the supply of oil. Restarting oil wells shut down at the beginning of the pandemic turned out to be a long and expensive process. It was not until mid-2022 that oil production almost reached the pre-pandemic level, reaching 98.8 million barrels per day.
([źródło](https://www.eia.gov/outlooks/steo/report/global_oil.php)).

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Range%20and%20median%20in%20PLN.png" width="550">
</p>
<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Range%20and%20median%20in%20USD.png" width="550">
</p>

When it finally seemed that the growth period was over, Russia's aggression against Ukraine caused another price shock. The price per barrel soared to over $120. In the meantime, the Polish złoty was weakening dynamically. In 2021, this was due to, among other things, the ongoing problems in Europe caused by the pandemic, the conflict between the Polish government and the European Union regarding funds for the National Reconstruction Plan, the inept communication of the National Bank of Poland, the global outflow of capital to the dollar. In 2022, the above factors were joined by: the outbreak of war in Ukraine, the first signs of economic weakening and high inflation. Such conditions caused the purchase of 1 barrel of oil to increase drastically. Contrary to the situation during the financial crisis of 2008, this time the złoty exposed the Polish economy to global economic problems. During the previous crisis, due to the source of the crisis in the USA, the złoty appreciated, thus cushioning the increase in oil prices. The chart shows the increase in the price of Brent oil expressed in USD and PLN compared to 2005. In the case of the dollar, the increase is already very large. However, against the background of the Polish currency, it turns out that the situation may be even worse.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Wholesale%20vs%20brent%20price.png" width="550">
</p>

The above chart allows us to notice certain relationships between the wholesale price and Brent quotations. As time passed, wholesale prices began to diverge more and more from Brent prices, which was quickly labeled in the media as the result of excessive refinery profits and the greed of corporate CEOs. In fact, the deviation began to grow, which can be explained by a number of other factors. These are the war and all its consequences: a reduction in the purchase of Russian oil, a sudden increase in demand, rising inflation affecting the increase in operating costs, an increase in interest rates, an increase in other production factors, including natural gas and electricity, and an increase in fuel consumption due to the lifting of pandemic restrictions and a return to the pre-pandemic demand trend.

<p align="center">
<img src="htthttps://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Price-generating%20factors%20Pb95.png" width="550">
</p>
<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Model%20vs%20actual%20price.png" width="550">
</p>

The last of the mentioned factors may be part of the answer to the increases from the April-June period. At that time, there was a significant increase in demand for fuel on the Polish market, which in the current macroeconomic environment easily leads to a state of market imbalance and, consequently, an excessive increase in product prices. On the one hand, it can be noted that the refinery processing capacity for the Orlen Group alone is 35.2 million tons, and for Lotos 5 million tons, which covers the current level of consumption in Poland. On the other hand, these values ​​are maximum possibilities and achieving them requires time and a decrease in the refinery's efficiency, which translates into the price. For this reason, in 2020, domestic production amounted to "only" 26.3 and 27.2 million tons for 2021. On the sidelines, it should be noted that domestic production is partially exported (a few %), which means that ultimately less fuel is available on the Polish market. Consumption for these years was 32.7 and 35 million tons, respectively. Excessive demand has so far been supplemented by imports from abroad, often from the East. For many years, Russia has been the main source of crude oil imports. In 2021, 60.9% of oil came from Russia. This share indicates a significant dependence on supplies from this country, which, along with the development of the war in Ukraine, ultimately led to an excessive increase in prices due to limited processing capacities. The Office of Competition and Consumer Protection (UOKiK) confirms the arguments cited with its own [analysis](https://uokik.gov.pl/aktualnosci.php?news_id=18755), while also informing about the significant impact of China's restrictions on fuel exports in 2022.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Range%20in%20PLN.png" width="550">
</p>
<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Range%20in%20USD.png" width="550">
</p>

The range of oil price fluctuations shown in the above chart allows us to identify how the exchange rate affects the price per barrel. In the case of Poland, the 2008 financial crisis did indeed lead to a significant increase in oil price fluctuations, but a much higher increase was recorded in 2020 and 2022. In the case of the USA, it was 2008 that was the period when fluctuations were the greatest. Detailed charts showing the movements of oil expressed in PLN and USD are included below.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Brent%20in%20PLN.png" width="550">
</p>
<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Brent%20USD.png" width="550">
</p>

## Price of PB95 among EU Member States

The petrol price shown below shows the range of movement of the average petrol price for European countries. The following conclusions can be read from the graph:
* Countries consistently maintain higher or lower prices relative to other countries. For example, Poland consistently maintains the position of countries with lower petrol prices, while in Germany the price is higher.
* The petrol price in Malta is constant with periodic changes. Malta has guaranteed itself price stability through an agreement signed in 2014 with the Azerbaijani company SOCAR Energy, which, given the current market conditions, this agreement favors the residents of Malta [source]( https://www.energylivenews.com/2022/02/17/how-is-malta-swerving-energy-prices/ ).
* As a result of the political decisions of the Hungarian government, the petrol price in Hungary in 2022 is not moving in the same direction as other EU member states.
* The petrol price in Austria was similar to the level recorded in Poland. This only changed in 2022.
* In Germany, the average price of petrol once again exceeded EUR 2 in September 2022. The cost of filling up with 1 litre of petrol in Germany is one of the highest in the entire EU.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Pb95%20price%20among%20EU%20members.png" width="550">
</p>

The study of price changes in relation to 2021 allows us to draw additional conclusions:
- Austria recorded the largest increase in petrol prices
- The price of petrol in Germany in August and September was in an upward trend
- Retail prices in Hungary, starting from the outbreak of the war in Ukraine, stopped responding to the market situation in a similar way to other EU countries
- There was no increase in the price of petrol in Malta
- Poland recorded a small increase in the price of petrol compared to other countries. However, it should be noted that in February 2022, the price in Poland decreased as a result of the entry into force of the so-called Anti-Inflation Shield 2, which reduced the VAT tax on fuel from 23% to 8%.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Increase%20in%20Pb95%20fuel%20prices%20among%20EU%20members.png" width="550">
</p>

The presentation of the level of price growth over the year on a map does not suggest the occurrence of clustering of similar values ​​in space. Differences in the scope of changes may be caused by a number of factors, the key ones of which include: diversification of oil suppliers, infrastructure of oil transshipment terminals, fiscal and international policy.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Annual%20change%20in%20gasoline%20spending%20in%20Europe.png" width="550">
</p>

An interesting spatial analysis is also the presentation of the current (as of September 19, 2022) average price on a map. It turns out that the grouping of countries with similar prices seems to be small. The price of gasoline is the highest for Scandinavian countries, Germany, the Netherlands, and Greece. The lowest prices can be found at stations in Poland, Hungary, Slovenia, and Croatia. However, this is not at all encouraging information for these countries due to the lower level of average earnings and a larger share of expenditure on energy products in total expenditure. These issues mean that Poland and other countries of Central and Eastern Europe are even more affected by price changes than Western European countries, which is visible in the previous graph of the annual change in gasoline expenditure in Europe.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Gasoil%20prices%20in%20Europe%20on%202022-08-29.png" width="550">
</p>

Visualizing price levels on a map for 2021 allows you to see the scope of changes that have occurred over the year. For data as of August 30, 2021, there is a clustering of high-price countries in the Western Europe and Nordic regions and low-price countries in the Central and Eastern Europe region.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Gasoil%20prices%20in%20Europe%20on%202021-08-23.png" width="550">
</p>


## Inflation

The current study allows us to draw attention to the impact of changes in energy and food prices on inflation. The share presented in the legend indicates the source of inflation. In countries such as Germany, Spain, Romania, Belgium, and Latvia, inflation is mainly the result of an increase in energy and food prices. Due to differences in petrol expenditure in individual countries, the increases do not affect the inflation rate in the same way. In those countries where the share is lower, this may indicate two options. The first is that energy and food price inflation has "spill over" to other inflation categories, so inflation is also the result of an increase in core inflation. The second possibility is the lower weight of products included in this category in the inflation basket, which is the domain of countries with high income per person. The share of energy and food price increases for the Scandinavian countries may be lower for this reason. This topic will be explored in more detail later in the study.  

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Share%20of%20YoY%20increase%20in%20energy%20and%20food%20prices%20on%20HICP%20inflation.png" width="550">
</p>
<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20HICP%20in%20Europe.png" width="550">
</p>

Inflation according to the HICP index is presented in the above chart. At first glance, one can notice autocorrelation, i.e. the grouping of countries with higher inflation in Eastern Europe. The area of ​​Western Europe, except for the countries located on the Iberian Peninsula, experiences lower price growth. Scandinavian countries (excluding Denmark) also belong to the group of countries with lower inflation.

The correlation of the extent of price increases with the share of energy price increases in inflation allows us to determine the source of inflation. In the case of countries marked in red, inflation is at a higher level than other countries, and the price increase is not caused in these countries by an increase in the prices of energy resources. Inflation in these countries is at a high level and is fueled by growth in other products and/or services. For countries marked in blue, inflation is at a low level and the share of energy price increases is high. This result is possible due to the low weight of expenditure on products and/or services included in this category. Countries belonging to this category are usually characterized by a higher GDP per capita.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20and%20share%20of%20YoY%20increase%20in%20energy%20and%20food%20prices%20in%20HICP%20inflation%20in%20Europe.png" width="550">
</p>

The next map shows inflation compared to core inflation. Thus, it can be concluded that high inflation in CEE countries is also due to the spillover of energy price increases to consumer products and/or services. High core inflation is more difficult to slow down and may remain relatively stable in the medium term. Countries in the east of the EU may therefore experience a longer period of price increases.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20and%20base%20inflation%20in%20Europe.png`" width="550">
</p>

This time, the map shows the relationship between inflation and GDP per capita adjusted for purchasing power. Again, the clustering of countries is visible. Countries in red belong to the group with high inflation and lower GDP per capita. These countries are particularly affected by inflation.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20and%20purchasing%20power%20adjusted%20GDP%20per%20capita%20in%20Europe.png" width="550">
</p>

The graphs of inflation over time presented above provide further insights. Inflation is broken down by criteria into services, energy, non-energy manufactured goods, and food, including alcohol and tobacco. In most countries, inflation began to accelerate before the start of 2022. Many continue to see sharp increases without a visible decline. The highest inflation is experienced by the Baltic countries: Lithuania, Latvia, and Estonia.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20in%20all%20countries.png" width="550">
</p>

Limiting the number of countries to Austria, Germany, Poland and Italy, we can see a significant share of energy and food in inflation. Of these countries, only Poland is also experiencing price increases in services and non-energy industrial goods. In 2020, inflation in Poland was mainly driven by services. It is interesting that Italy is not experiencing price increases in these categories, which at first glance is the result of the country's demographic structure - a relatively large share of older people in society compared to other EU countries.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20in%20selected%20countries.png" width="550">
</p>

The breakdown of inflation into all of Eurostat's coicop categories allows us to definitively determine the source of inflation. These are primarily: food and non-alcoholic beverages, housing, water, electricity, gas and other fuels, transport, and restaurants and hotels. All of these categories are dependent on fuel prices on the retail market. Low levels for the remaining categories are positive news, as along with inflation, labor costs, the costs of producing non-energy products, and transport also increase. So far, wages in Poland have followed inflation, but this trend will probably soon be interrupted with falling demand and lower bargaining power of employees on the labor market.

<p align="center">
<img src="https://github.com/Vosbrucke/Poland_Pb95_prices/blob/main/Plots/Inflation%20components.png" width="550">
</p>

## Summary

In between Nov 2021 and Nov 2022 petrol prices in Poland have increased by over 30% exceeding in the meantime PLN 8 per litre. Pb95 has not been so expensive in Poland since 2014. The high price is mainly the result of the war waged by Russia in Ukraine. The reason for this state of affairs is the increase in the prices of Brent Oil resulting from the sanctions imposed on Russia as a result of the war in Ukraine and Poland's relations with Ukraine, which contributed to the appreciation of the złoty against the US dollar and the euro. In the media at the turn of May and June, accusations appeared of an excessive increase in the wholesale price compared to the Brent price expressed in the złoty, which actually took place at that time. Public opinion indicated the manipulation of the prices of petroleum products by Polish refineries in order to achieve higher profits as the cause. However, the analysis presents other reasons for this situation. The source of the "detachment" of the retail price from the wholesale price can be seen in the exceptional situation on the fuel market in the form of a sudden interruption of supplies of crude oil from Russia. This led to increased demand, which could not be immediately addressed by an increase in supply. Although the production capacities of oil refineries are able to cover the demand for fuel in Poland within a year, increasing their production is a time- and cost-consuming process, which translates into an increase in prices. The analysis conducted by the Office of Competition and Consumer Protection shows that the price increase is caused by objective market factors.

In the period under review, changes in the price of crude oil were also inflationary. Its increase led to a varying degree to an increase in inflation in EU countries. The countries most affected by the increase are the countries of Central and Eastern Europe, where the share of expenditure on energy-intensive products is higher than in Western Europe and the Scandinavian countries. In the case of countries located in the east of the EU, the price increase also applies to non-energy products and/or services. The increase in the prices of energy raw materials and food thus increases core inflation. Another key issue is the actions taken by central banks and governments of individual countries, particularly important for countries with their own national currencies. In 2008, during the financial crisis, the Polish złoty cushioned the shocks of the global economy - the PLN appreciated against the EUR and USD at that time, so the change in oil prices was not as large for Poland. Currently, however, it is the other way around. Not only is there a war going on just across the Polish border, but investors also have reservations about the credibility of the NBP's monetary policy and the PiS government's fiscal policy. Another problem is the dispute with the EU over money from the National Reconstruction Plan. These are other factors that contribute to the weakening of the złoty against the US dollar and the euro, which stimulates inflation.