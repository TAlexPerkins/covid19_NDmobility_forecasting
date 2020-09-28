# covid19_NDmobility_forecasting

This forecast is an ensemble of nine models that are identical except that they are driven by different mobility indices from Apple and Google. The model underlying each is a deterministic, SEIR-like model.

To train each model (from the code folder):
1. Rscript gather_data.R
2. qsub submit_training.sh
3. Rscript generate_ensemble_weights.R

To generate whole ensemble (after training, from the code folder):
1. Rscript gather_data.R
2. qsub submit.sh
3. qsub submit_forecasts.sh
