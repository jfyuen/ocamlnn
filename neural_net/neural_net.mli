module Make : functor (Nn : Model.NEURAL_NET) ->
  (Model.MODEL with type neural_net = Nn.neural_net)

module Backpropagation : Model.NEURAL_NET with type neural_net = Backprop.neural_net
  
module MBackpropagation :
  Model.MODEL with type neural_net = Backpropagation.neural_net
