import { ABC_SUCCESS, ABC_PARSE_ERROR, ABC_RUNTIME_ERROR } from './constants';
const initialState = {
  abc: '',
  parse_error: '',
  runtime_error: '',
};

const mainReducer = (state = initialState, action) => {
  const { type, payload } = action;
  switch (type) {
    case ABC_SUCCESS:
      return { ...state, abc: payload, parse_error: '', runtime_error: '' };
    case ABC_PARSE_ERROR:
      return { ...state, parse_error: payload, runtime_error: '' };
    case ABC_RUNTIME_ERROR:
      return { ...state, parse_error: '', runtime_error: payload };
    default:
      return state;
  }
};

export default mainReducer;
