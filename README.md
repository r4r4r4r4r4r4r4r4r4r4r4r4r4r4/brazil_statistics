### Quantifying spatio-temporal risk from TB in Brazil


[25 marks] Clear exposition of the steps you took in model fitting and exposition of a final model.

\noindent Steps -
\begin{itemize}
    \item initial plots of the variables to determine plausible important features (txt file in github)
    \item first gam model built (poisson family as count data, and a select few important features)
    \item assessed the summary to determine feature importance, observed residuals (gam.check) for fitness of model and used the output to assess number of knots
    \item assessed the smooth term plots of the model (plot(model)) to further understand feature importance
    \item made model iteratively more complex, repeating the above steps each time to determine whether added complexity improved the model (worth including some example code of earlier models with comments on what was changed perhaps?)
    \item describe the final model and then the tweaking of knots values to get k index as close to 1 as possible and showing that the p values were significant (may be worth including these statistics in a 'final model table', as well as maybe aspects of the summary. Residual plots and smooth term plots can be added at the end as figures, although it would look better within the text so maybe email about that?
\end{itemize}
