
class GainProcessor extends AudioWorkletProcessor {

  // Custom AudioParams can be defined with this static getter.
  static get parameterDescriptors() {
    return [{ name: 'gain', defaultValue: 1 }];
  }

  constructor() {
    // The super constructor call is required.
    super();
  }

  process(inputs, outputs, parameters) {
    const input = inputs[0];
    const output = outputs[0];
    const gain = parameters.gain;
    for (let channel = 0; channel < input.length; ++channel) {
      const inputChannel = input[channel];
      const outputChannel = output[channel];
      if (gain.length === 1) {
        for (let i = 0; i < inputChannel.length; ++i)
          outputChannel[i] = inputChannel[i] * gain[0];
      } else {
        for (let i = 0; i < inputChannel.length; ++i)
          outputChannel[i] = inputChannel[i] * gain[i];
      }
    }

    return true;
  }
}

registerProcessor('gain-processor', GainProcessor);

/* class EqualProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[i] == input2[i]) output[i] = 1; else output[i] = 0;
    }
    return true;
  }
}
registerProcessor('equal-processor',EqualProcessor);


class NotEqualProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[i] != input2[i]) output[i] = 1; else output[i] = 0;
    }
    return true;
  }
}
registerProcessor('notEqual-processor',NotEqualProcessor);


class GreaterThanProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[i] > input2[i]) output[i] = 1; else output[i] = 0;
    }
    return true;
  }
}
registerProcessor('greaterThan-processor',GreaterThanProcessor);


class GreaterThanOrEqualProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[i] >= input2[i]) output[i] = 1; else output[i] = 0;
    }
    return true;
  }
}
registerProcessor('greaterThanOrEqual-processor',GreaterThanOrEqualProcessor);


class LessThanProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[i] < input2[i]) output[i] = 1; else output[i] = 0;
    }
    return true;
  }
}
registerProcessor('lessThan-processor',LessThanProcessor);


class LessThanOrEqualProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [];
  }
  constructor() {
    super();
  }
  process(inputs,outputs,parameters) {
    const input1 = inputs[0];
    const input2 = inputs[1];
    const output = outputs[0];
    for(let i = 0; i < input1[0].length; i++) {
      if(input1[i] <= input2[i]) output[i] = 1; else output[i] = 0;
    }
    return true;
  }
}
registerProcessor('lessThanOrEqual-processor',LessThanOrEqualProcessor);
*/
