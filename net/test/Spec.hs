import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "weightedSum" $ do
    it "should apply weights to inputs and apply the bias" $ do
      weightedSum 0.6 [0.3, 0.4] [0.1, 0.2] `shouldBe` 0.71

  describe "Neuron" $ do
    it "should evaluate correctly the ouput given inputs, weights, aggregagtion and activation functions" $ do
      ((Neuron ([0.1, 0.2], (weightedSum 0.6), sigmoid)) `evaluateNeuron` [0.3, 0.4])  `shouldBe` 0.6704011598088686

  describe "Layer" $ do
    it "should evaluate correctly the ouputs given a list of neurons" $ do
      let neuron1 = (Neuron ([0.15, 0.2], (weightedSum 0.35), sigmoid))
      let neuron2 = (Neuron ([0.25, 0.3], (weightedSum 0.35), sigmoid))
      ([neuron1, neuron2] `evaluateLayer` [0.05, 0.10]) `shouldBe` [0.5932699921071872,0.596884378259767]


  describe "Network" $ do
    it "should evaluate correctly the ouputs given a list of layers" $ do
      let h1 = (Neuron ([0.15, 0.2], (weightedSum 0.35), sigmoid))
      let h2 = (Neuron ([0.25, 0.3], (weightedSum 0.35), sigmoid))
      let o1 = (Neuron ([0.4, 0.45], (weightedSum 0.6),  sigmoid))
      let o2 = (Neuron ([0.5, 0.55], (weightedSum 0.6),  sigmoid))
      let outputLayer = [o1, o2]
      let hiddenLayer = [h1, h2]
      ([hiddenLayer, outputLayer] `evaluateNetwork` [0.05, 0.10]) `shouldBe` [[0.05,0.1], [0.5932699921071872,0.596884378259767], [0.7513650695523157,0.7729284653214625]]

    it "should determine the error of output" $ do
      derivationErrorForOutputNeuron 0.75136507 0.01 `shouldBe` 0.74136507

    it "should determine the error of activation function" $ do
      errorIntroducedByActivationFunction  (Neuron ([0.4, 0.45], (weightedSum 0.6),  sigmoid)) [0.5932699921071872,0.596884378259767] 0.7513650695523157 `shouldBe` 0.18681560180895948

    it "should learn for output layer" $ do    
      let o1 = (Neuron ([0.4, 0.45], (weightedSum 0.6),  sigmoid))
      let o2 = (Neuron ([0.5, 0.55], (weightedSum 0.6),  sigmoid))
      let outputLayer = [o1, o2]
      let initialNetwork = [outputLayer]
      let expected_o1 = (Neuron ([0.35891647971788465, 0.4086661860762334], (weightedSum 0.6), sigmoid))
      let expected_o2 = (Neuron ([0.5113012702387375,  0.5613701211079891], (weightedSum 0.6), sigmoid))
      let expected_outputLayer = [expected_o1, expected_o2]
      let expected_network = [expected_outputLayer]
      let allOutputs = [[0.5932699921071872,0.596884378259767], [0.7513650695523157,0.7729284653214625]]
      (backpropagation initialNetwork allOutputs [0.01, 0.99] 0.5) `shouldBe` expected_network

    it "should learn for hidden layer" $ do    
      let h1 = (Neuron ([0.15, 0.2], (weightedSum 0.35), sigmoid))
      let h2 = (Neuron ([0.25, 0.3], (weightedSum 0.35), sigmoid))
      let hiddenLayer = [h1, h2]
      let o1 = (Neuron ([0.35891647971788465, 0.4086661860762334], (weightedSum 0.6), sigmoid))
      let o2 = (Neuron ([0.5113012702387375,  0.5613701211079891], (weightedSum 0.6), sigmoid))
      let outputLayer = [o1, o2]
      let expected_h1 = (Neuron ([0.14981763856120295, 0.19963527712240592], (weightedSum 0.35), sigmoid))
      let expected_h2 = (Neuron ([0.2497881851977662,  0.29957637039553237], (weightedSum 0.35), sigmoid))
      let expected_hiddenLayer = [expected_h1, expected_h2]
      (fst $ backpropagationForHiddenLayer hiddenLayer outputLayer ([0.05,0.1], [0.5932699921071872,0.596884378259767]) [0.13849856162855698,-3.809823651655623e-2] 0.5) `shouldBe` expected_hiddenLayer

    it "should learn for the whole network" $ do
      let h1 = (Neuron ([0.15, 0.2], (weightedSum 0.35), sigmoid))
      let h2 = (Neuron ([0.25, 0.3], (weightedSum 0.35), sigmoid))
      let o1 = (Neuron ([0.4, 0.45], (weightedSum 0.6),  sigmoid))
      let o2 = (Neuron ([0.5, 0.55], (weightedSum 0.6),  sigmoid))
      let outputLayer = [o1, o2]
      let hiddenLayer = [h1, h2]
      let initialNetwork = [hiddenLayer, outputLayer]
      let expected_h1 = (Neuron ([0.14981763856120295,0.19963527712240592],  (weightedSum 0.35), sigmoid))
      let expected_h2 = (Neuron ([0.2497881851977662,0.29957637039553237],   (weightedSum 0.35), sigmoid))
      let expected_o1 = (Neuron ([0.35891647971788465,0.4086661860762334],  (weightedSum 0.6),  sigmoid))
      let expected_o2 = (Neuron ([0.5113012702387375,0.5613701211079891], (weightedSum 0.6),  sigmoid))
      let expected_outputLayer = [expected_o1, expected_o2]
      let expected_hiddenLayer = [expected_h1, expected_h2]
      let expected_network = [expected_hiddenLayer, expected_outputLayer]
      let allOutputs = [[0.05,0.1], [0.5932699921071872,0.596884378259767], [0.7513650695523157,0.7729284653214625]]
      (backpropagation initialNetwork allOutputs [0.01, 0.99] 0.5) `shouldBe` expected_network