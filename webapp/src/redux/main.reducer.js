import { ABC_REQUESTED } from './constants';
const initialState = {
  abc: '',
};

const mainReducer = (state = initialState, action) => {
  const { type, payload } = action;
  switch (type) {
    case ABC_REQUESTED:
      return { ...state, abc: payload };
    default:
      return state;
  }
};

export default mainReducer;
