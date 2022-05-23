import { ABC_SUCCESS, ABC_PARSE_ERROR, ABC_RUNTIME_ERROR } from './constants';
import mainService from './main.service';

export const getAbc = (abc) => async (dispatch) => {
  try {
    const res = await mainService.getAbc({ Text: abc });
    console.log(res);
    dispatch({
      type: ABC_SUCCESS,
      payload: res.data,
    });

    return Promise.resolve(res.data);
  } catch (err) {
    console.log(err);

    if (err.request.status == 400) {
      dispatch({
        type: ABC_PARSE_ERROR,
        payload: err.response.data,
      });
    } else if (err.request.status == 406) {
      dispatch({
        type: ABC_RUNTIME_ERROR,
        payload: err.response.data,
      });
    }

    return Promise.reject(err);
  }
};
