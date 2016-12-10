module Lib
    ( someFunc,
      weightedSum,
      evaluateNeuron,
      sigmoid,
      evaluateLayer,
      evaluateNetwork,
      derivationErrorForOutputNeuron,
      derivationErrorForHiddenNeurons,
      errorIntroducedByActivationFunction,
      backpropagationForOuputNeuron,
      backpropagationForOuputLayer,
      backpropagationForHiddenLayer,
      backpropagation,
      Neuron(..),
      Layer,
      Network
    ) where

import Data.List

weightedSum :: Double -> [Double] -> [Double] -> Double
weightedSum bias weights = (+bias) . sum . zipWith (*) weights

sigmoid :: Double -> Double
sigmoid x = 1/(1+ exp (-x))

sigmoid' :: [Double] -> Double -> Double
sigmoid' _ x = (1 - x) * x

derive :: (Double -> Double) -> ([Double] -> Double -> Double)
derive sigmoid = sigmoid'

data Neuron = Neuron ([Double], ([Double] -> [Double] -> Double) , (Double -> Double)) 
instance Eq Neuron where
  (Neuron (w1, _, _)) == (Neuron (w2, _, _)) = (w1 == w2)

instance Show Neuron where
  show (Neuron (w1, _, _)) = show w1

evaluateNeuron :: Neuron -> [Double] -> Double
evaluateNeuron (Neuron (weights, aggregation, activation))  = activation . aggregation weights

type Layer = [Neuron]
evaluateLayer :: Layer -> [Double] -> [Double]
evaluateLayer neurons inputs = map (\neuron -> evaluateNeuron neuron inputs) neurons

type Network = [Layer]
evaluateNetwork :: Network -> [Double] -> [[Double]]
evaluateNetwork layers inputs = foldl (\acc layer -> (acc ++ [evaluateLayer layer (last acc)] ) ) [inputs] layers

derivationErrorForOutputNeuron :: Double -> Double -> Double
derivationErrorForOutputNeuron output expectedOutput = -(expectedOutput - output)

errorIntroducedByActivationFunction :: Neuron -> [Double] -> Double -> Double
errorIntroducedByActivationFunction (Neuron (weights, aggregation, activation)) inputs output = derived inputs output
  where
    derived = derive activation

backpropagationForOuputNeuron :: Neuron -> [Double] -> Double -> Double -> Double -> (Neuron, Double)
backpropagationForOuputNeuron neuron inputs output expectedOutput learningRate = 
    ((Neuron (updatedWeights, aggregation, activation)), delta)
  where 
    (Neuron (weights, aggregation, activation)) = neuron
    delta = (errorIntroducedByActivationFunction neuron inputs output) * (derivationErrorForOutputNeuron output expectedOutput)
    updatedWeights = zipWith (-) weights (map (* (learningRate * delta) ) inputs)

backpropagationForOuputLayer :: Layer -> [Double] -> [Double] -> [Double] -> Double -> (Layer, [Double])
backpropagationForOuputLayer layer inputs outputs expectedOutputs learningRate = unzip $ zipWith3 (\neuron output expectedOutput -> backpropagationForOuputNeuron neuron inputs output expectedOutput learningRate) layer outputs expectedOutputs

derivationErrorForHiddenNeurons :: Layer -> [Double] -> [Double]
derivationErrorForHiddenNeurons nextLayer nextLayersDeltas =  
    map sum $ map (\associatedWeight -> zipWith (*) associatedWeight nextLayersDeltas) associatedWeights
  where
    associatedWeights = transpose $ map (\(Neuron (w, _, _)) -> w ) nextLayer

backpropagationForHiddenLayer :: Layer -> Layer -> ([Double], [Double]) -> [Double] -> Double -> (Layer, [Double])
backpropagationForHiddenLayer layer nextLayer (inputs, outputs) nextLayerDeltas learningRate = (updatedHiddenLayer, deltas)
  where
    errorsIntroducedByActivationFunction = map (\(neuron, output) -> errorIntroducedByActivationFunction neuron inputs output) $ zip layer outputs -- [Double]
    derivationErrorsForHiddenNeurons = derivationErrorForHiddenNeurons nextLayer nextLayerDeltas -- [Double]
    deltas = zipWith (*) errorsIntroducedByActivationFunction derivationErrorsForHiddenNeurons -- [Double]
    weights = map (\(Neuron (w, _, _)) -> w) layer -- [[Double]]
    variations = map (\d -> map (\i -> i * learningRate * d) inputs ) deltas -- > [[Double]]
    updatedWeights = zipWith (zipWith (-)) weights variations
    updatedHiddenLayer = (zipWith (\(Neuron (_, aggregation, activation)) w -> (Neuron (w, aggregation, activation)) )  layer updatedWeights) :: Layer

backpropagation :: Network -> [[Double]] -> [Double] -> Double -> Network
backpropagation network allOutputs expectedOutputs learningRate =
    fst $ unzip $ backpropagationInternal hiddenLayers [(updatedOutputLayer, deltas)] (inputsOfOutputLayer:allInputs) learningRate
  where
    (outputsOfOutputLayer:inputsOfOutputLayer:allInputs) = reverse allOutputs
    (outputLayer:hiddenLayers) = reverse network
    (updatedOutputLayer, deltas) = backpropagationForOuputLayer outputLayer inputsOfOutputLayer outputsOfOutputLayer expectedOutputs learningRate

backpropagationInternal :: [Layer] -> [(Layer, [Double])] -> [[Double]] -> Double -> [(Layer, [Double])]
backpropagationInternal [] updatedHiddenLayers _ _ = updatedHiddenLayers
backpropagationInternal (x:xs) updatedHiddenLayers allInputs learningRate = backpropagationInternal xs (updatedHiddenLayer:updatedHiddenLayers) (inputs:rest) learningRate 
  where
    (outputs:inputs:rest) = allInputs
    (nextLayer, nextLayersDeltas) = head updatedHiddenLayers
    updatedHiddenLayer = backpropagationForHiddenLayer x nextLayer (inputs, outputs) nextLayersDeltas learningRate

someFunc :: IO ()
someFunc = putStrLn "someFunc"

